{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Tie
  ( generate,
    Writer,
    fileWriter,
    withTestWriter,
  )
where

import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Data.Aeson.Types as Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.HashSet as HashSet
import qualified Data.OpenApi as OpenApi
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Yaml (decodeFileThrow)
import Prettyprinter (Doc, vsep)
import Prettyprinter.Internal (unsafeTextWithoutNewlines)
import System.FilePath (takeDirectory, (</>))
import Tie.Codegen.Cabal (codegenCabalFile)
import Tie.Codegen.Imports
  ( codegenExternalHaskellDependencies,
    codegenExtraApiModuleDependencies,
    codegenExtraResponseModuleDependencies,
    codegenModuleHeader,
    codegenResponseDependencies,
    codegenSchemaDependencies,
  )
import Tie.Codegen.Operation
  ( codegenOperation,
    codegenOperations,
  )
import Tie.Codegen.Request (codegenRequestAuxFile)
import Tie.Codegen.Response (codegenResponseAuxFile, codegenResponses)
import Tie.Codegen.Schema (codegenSchema)
import Tie.Name
  ( Name,
    additionalPropertiesTypeName,
    apiHaskellFileName,
    apiHaskellModuleName,
    cabalFileName,
    fromText,
    inlineArrayElementTypeName,
    inlineObjectTypeName,
    inlineVariantTypeName,
    requestHaskellFileName,
    requestHaskellModuleName,
    responseHaskellFileName,
    responseHaskellModuleName,
    toOperationHaskellFileName,
    toResponseHaskellFileName,
    toResponseHaskellModuleName,
    toSchemaHaskellFileName,
    toSchemaHaskellModuleName,
  )
import Tie.Operation
  ( Operation (..),
    errors,
    normalizeOperation,
    operationExternalDependencies,
    operationResponseDependencies,
    operationSchemaDependencies,
    pathItemsToOperation,
  )
import Tie.Resolve (newResolver)
import Tie.Type
  ( Named,
    Type,
    namedTypeDependencies,
    normalizeType,
    schemaToType,
    transitiveDependencies,
    typeDependencies,
    typeExternalDependencies,
  )
import Tie.Writer (Writer, fileWriter, withTestWriter)
import Prelude hiding (Type)

-- | Our own version of nubOrd that both nubs and sorts
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = Set.toList . Set.fromList

-- | Read an OpenAPI spec. Throws in case it can not
-- be read or deserialized.
readOpenApiSpec ::
  (MonadIO m) =>
  FilePath ->
  m OpenApi.OpenApi
readOpenApiSpec filePath = do
  value <- liftIO (decodeFileThrow filePath)
  value <- resolveFileReferences (takeDirectory filePath) value
  case Data.Aeson.parseMaybe Data.Aeson.parseJSON value of
    Just openApi ->
      pure openApi
    Nothing ->
      error "Could not decode OpenAPI specification"

resolveFileReferences ::
  (MonadIO m) =>
  FilePath ->
  Data.Aeson.Value ->
  m Data.Aeson.Value
resolveFileReferences cwd value = case value of
  Data.Aeson.Object object
    -- Relative references of the form
    --   "$ref": "./dir/some-file.yaml"
    | Just (Data.Aeson.String path) <- Data.Aeson.KeyMap.lookup "$ref" object,
      "." `Text.isPrefixOf` path -> do
        let fileName = cwd </> toString path
        value <- liftIO (decodeFileThrow fileName)
        resolveFileReferences (takeDirectory fileName) value

    -- Relative references of the form
    --   "$ref": "/dir/some-file.yaml"
    | Just (Data.Aeson.String path) <- Data.Aeson.KeyMap.lookup "$ref" object,
      "/" `Text.isPrefixOf` path -> do
        let fileName = toString path
        value <- liftIO (decodeFileThrow fileName)
        resolveFileReferences (takeDirectory fileName) value
    | otherwise ->
        Data.Aeson.Object <$> forM object (resolveFileReferences cwd)
  Data.Aeson.Array array ->
    Data.Aeson.Array <$> forM array (resolveFileReferences cwd)
  Data.Aeson.String {} ->
    pure value
  Data.Aeson.Number {} ->
    pure value
  Data.Aeson.Bool {} ->
    pure value
  Data.Aeson.Null ->
    pure value

-- | Extracts all the schemas form an 'OpenApi.OpenApi'.
specSchemas :: OpenApi.OpenApi -> [(Text, OpenApi.Schema)]
specSchemas =
  InsOrd.toList . OpenApi._componentsSchemas . OpenApi._openApiComponents

specPaths :: OpenApi.OpenApi -> [(FilePath, OpenApi.PathItem)]
specPaths =
  InsOrd.toList . OpenApi._openApiPaths

specComponents :: OpenApi.OpenApi -> OpenApi.Components
specComponents =
  OpenApi._openApiComponents

-- | Normalizes a 'Type' by extracting the contained inline type
-- definitions.
normalize :: (Monad m) => Name -> Type -> m (Type, [(Name, Type)])
normalize =
  normalizeType
    ( \enclosingType fieldName ->
        pure (inlineObjectTypeName enclosingType fieldName)
    )
    ( \enclosingType ->
        pure (additionalPropertiesTypeName enclosingType)
    )
    ( \enclosingType ith ->
        pure (inlineVariantTypeName enclosingType ith)
    )
    ( \enclosingType ->
        pure (inlineArrayElementTypeName enclosingType)
    )

-- | Expands a list of inline definitions until it reaches a fixed point.
-- The invariant of the returned list is that there are no non-primitive
-- unnamed types left:
--   forall x. normalize x == []
--  where x is an element of the result of normalizedTypes
normalizeTypes :: (Monad m) => [(Name, Type)] -> m [(Name, Type)]
normalizeTypes types =
  concat
    <$> traverse
      ( \(name, type_) -> do
          (normalizedType, inlineDefinitions) <- normalize name type_
          normalizedTypes <- normalizeTypes inlineDefinitions
          pure ((name, normalizedType) : normalizedTypes)
      )
      types

generate ::
  (MonadIO m) =>
  Writer m ->
  -- | Package name
  Text ->
  -- | Module name
  Text ->
  -- | Extra cabal packages
  [Text] ->
  FilePath ->
  m ()
generate write packageName apiName extraPackages inputFile = do
  openApi <- readOpenApiSpec inputFile

  -- Helper to resolve components in the spec.
  let resolver =
        newResolver
          (specComponents openApi)
          (\ref -> error ("could not resolve reference " <> show ref))

  -- Extract all the Operations from the spec
  operations' <-
    pathItemsToOperation
      resolver
      errors
      (specPaths openApi)
  let operations =
        sortOn
          (\Operation {name} -> name)
          operations'

  -- Only extract the direct, shallow dependencies. This is used to get a precise
  -- import list for the api and schema modules.
  let shallow :: Named Type -> [Name]
      shallow =
        namedTypeDependencies

  -- Deeply traverse a type and extracts all dependencies. Used to get a list
  -- of all the things we have to generate.
  let transitive :: Named Type -> [Name]
      transitive =
        transitiveDependencies

  -- Transitive closure of all the referenced Schemas
  let allReferencedSchemas :: HashSet.HashSet Name
      allReferencedSchemas =
        HashSet.fromList $
          foldMap (operationSchemaDependencies transitive) operations

  -- Walk through all the available Schemas and generate code for the
  -- referenced ones.
  for_ (specSchemas openApi) $ \(name, schema) -> do
    let name' = fromText name
        path = toSchemaHaskellFileName apiName name'
        header = codegenModuleHeader (toSchemaHaskellModuleName apiName name')
    when (name' `HashSet.member` allReferencedSchemas) $ do
      type_ <- schemaToType resolver schema
      let schemaDependencyCode =
            codegenSchemaDependencies apiName $
              nubOrd (typeDependencies shallow type_)
          externalDependencyCode =
            codegenExternalHaskellDependencies $
              nubOrd (typeExternalDependencies type_)
      -- Extract inline dependencies after dependency analysis. We
      -- will generate the code for the inline dependencies in the
      -- same file
      (normedType, inlineDependencies) <-
        normalize name' type_
      codeForInlineDependencies <-
        traverse (uncurry codegenSchema) inlineDependencies
      -- Generate code for the schema
      output <-
        codegenSchema name' normedType
      write path $
        vsep $
          intersperse mempty $
            concat
              [ [ header,
                  externalDependencyCode,
                  schemaDependencyCode
                ],
                codeForInlineDependencies,
                [output]
              ]

  -- For each Operation, generate data types for the responses.
  for_ operations $ \operation@Operation {name} -> do
    let path = toResponseHaskellFileName apiName name
        header = codegenModuleHeader (toResponseHaskellModuleName apiName name)
        importsCode =
          codegenSchemaDependencies apiName $
            nubOrd (operationSchemaDependencies shallow operation)
    (operation, inlineDefinitions) <-
      normalizeOperation operation
    -- normalizeOperation doesn't recurse into transitive inline definitions,
    -- we apply normalizeTypes explicitly to normalize transitive inline definitions
    -- explicitly
    normalizedInlineDefinitions <-
      normalizeTypes inlineDefinitions
    codeForInlineDefinitions <-
      traverse (uncurry codegenSchema) normalizedInlineDefinitions
    responsesCode <-
      codegenResponses
        resolver
        (responseHaskellModuleName apiName)
        operation
    write path $
      vsep $
        intersperse mempty $
          concat
            [ [ header,
                importsCode,
                codegenExtraResponseModuleDependencies apiName
              ],
              codeForInlineDefinitions,
              [responsesCode]
            ]

  -- Generate auxliary definitions in Response.hs
  let path = responseHaskellFileName apiName
  write path $
    unsafeTextWithoutNewlines $
      codegenResponseAuxFile (responseHaskellModuleName apiName)

  -- Generate auxliary definitions in Request.hs
  let path = requestHaskellFileName apiName
  write path $
    unsafeTextWithoutNewlines $
      codegenRequestAuxFile (requestHaskellModuleName apiName)

  -- Generate a single Api.hs module containing the server for the api

  -- Normalize operations, to give all anonymous types a name
  normalizedOperations <-
    traverse
      (fmap fst . normalizeOperation)
      operations

  -- Generate operations code form the normalized representation
  -- Careful: We still want the imports and dependencies be dependent
  -- only
  operationsCode <-
    codegenOperations
      resolver
      normalizedOperations

  let path = apiHaskellFileName apiName

      header =
        codegenModuleHeader (apiHaskellModuleName apiName)
      schemaDependencyCode =
        codegenSchemaDependencies apiName $
          nubOrd $
            concatMap
              (operationSchemaDependencies shallow)
              operations
      externalDependencyCode =
        codegenExternalHaskellDependencies $
          nubOrd $
            concatMap
              operationExternalDependencies
              operations
      responseDependencyCode =
        codegenResponseDependencies apiName $
          nubOrd $
            concatMap
              operationResponseDependencies
              operations

  write path $
    vsep $
      intersperse
        mempty
        [ header,
          codegenExtraApiModuleDependencies apiName,
          externalDependencyCode,
          schemaDependencyCode,
          responseDependencyCode,
          operationsCode
        ]

  -- Last but not least, generate the Cabal file
  let allReferencedModules :: [Text]
      allReferencedModules =
        nubOrd $
          map (toSchemaHaskellModuleName apiName) (toList allReferencedSchemas)
            ++ foldMap (map (toResponseHaskellModuleName apiName) . operationResponseDependencies) operations
            ++ [ apiHaskellModuleName apiName,
                 responseHaskellModuleName apiName,
                 requestHaskellModuleName apiName
               ]

      path = cabalFileName packageName
  write path (codegenCabalFile packageName allReferencedModules extraPackages)
