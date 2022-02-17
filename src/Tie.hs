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

import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.OpenApi as OpenApi
import qualified Data.Set as Set
import Data.Yaml (decodeFileThrow)
import Prettyprinter (Doc, vsep)
import Tie.Codegen.Cabal (codegenCabalFile)
import Tie.Codegen.Imports
  ( codegenExtraApiModuleDependencies,
    codegenExtraResponseModuleDependencies,
    codegenModuleHeader,
    codegenResponseDependencies,
    codegenSchemaDependencies,
  )
import Tie.Codegen.Operation
  ( codegenOperation,
    codegenOperations,
  )
import Tie.Codegen.Response (codegenResponseAuxFile, codegenResponses)
import Tie.Codegen.Schema (codegenSchema)
import Tie.Name
  ( Name,
    apiHaskellFileName,
    apiHaskellModuleName,
    cabalFileName,
    fromText,
    inlineObjectTypeName,
    inlineVariantTypeName,
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
  )
import Tie.Writer (Writer, fileWriter, withTestWriter)
import Prelude hiding (Type)

-- | Our own version of nubOrd that both nubs and sorts
nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList

-- | Read an OpenAPI spec. Throws in case it can not
-- be read or deserialized.
readOpenApiSpec ::
  MonadIO m =>
  FilePath ->
  m OpenApi.OpenApi
readOpenApiSpec filePath =
  liftIO (decodeFileThrow filePath)

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
normalize :: Monad m => Name -> Type -> m (Type, [(Name, Type)])
normalize =
  normalizeType
    ( \enclosingType fieldName ->
        pure (inlineObjectTypeName enclosingType fieldName)
    )
    ( \enclosingType ith ->
        pure (inlineVariantTypeName enclosingType ith)
    )

generate :: MonadIO m => Writer m -> FilePath -> m ()
generate write inputFile = do
  -- TODO make configurable
  let apiName = "OpenAPI"
      packageName = "open-api"

  openApi <- readOpenApiSpec inputFile

  -- Helper to resolve components in the spec.
  let resolver =
        newResolver
          (specComponents openApi)
          (\_ -> error "could not resolve reference")

  -- Extract all the Operations from the spec
  operations <-
    pathItemsToOperation
      resolver
      errors
      (specPaths openApi)

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
  let allReferencedSchemas :: [Name]
      allReferencedSchemas =
        foldMap (operationSchemaDependencies transitive) operations

  -- Walk through all the available Schemas and generate code for the
  -- referenced ones.
  for_ (specSchemas openApi) $ \(name, schema) -> do
    let name' = fromText name
        path = toSchemaHaskellFileName apiName name'
        header = codegenModuleHeader (toSchemaHaskellModuleName apiName name')
    when (name' `elem` allReferencedSchemas) $ do
      type_ <- schemaToType resolver schema
      let dependencyCode =
            codegenSchemaDependencies apiName $
              nubOrd (typeDependencies shallow type_)
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
        vsep
          [ header,
            mempty,
            dependencyCode,
            mempty,
            vsep (intersperse mempty codeForInlineDependencies),
            mempty,
            output
          ]

  -- For each Operation, generate data types for the responses.
  for_ operations $ \operation@Operation {name} -> do
    let path = toResponseHaskellFileName apiName name
        header = codegenModuleHeader (toResponseHaskellModuleName apiName name)

        dependencyCode =
          codegenSchemaDependencies apiName $
            nubOrd (operationSchemaDependencies shallow operation)
    responsesCode <- codegenResponses resolver operation
    write path $
      vsep
        [ header,
          mempty,
          dependencyCode,
          mempty,
          codegenExtraResponseModuleDependencies apiName,
          mempty,
          responsesCode
        ]

  -- Generate auxiliary definitions in Response.hs
  let path = responseHaskellFileName apiName
      header = codegenModuleHeader (responseHaskellModuleName apiName)

  write path $
    vsep
      [ header,
        mempty,
        codegenResponseAuxFile
      ]

  -- Generate a single Api.hs module containing the server for the api
  operationsCode <- codegenOperations resolver operations
  let path = apiHaskellFileName apiName

      header =
        codegenModuleHeader (apiHaskellModuleName apiName)
      schemaDependencyCode =
        map
          (codegenSchemaDependencies apiName . nubOrd . operationSchemaDependencies shallow)
          operations
      responseDependencyCode =
        map
          (codegenResponseDependencies apiName . nubOrd . operationResponseDependencies)
          operations

  write path $
    vsep
      [ header,
        mempty,
        codegenExtraApiModuleDependencies apiName,
        mempty,
        vsep schemaDependencyCode,
        mempty,
        vsep responseDependencyCode,
        mempty,
        operationsCode
      ]

  -- Last but not least, generate the Cabal file
  let allReferencedModules :: [Text]
      allReferencedModules =
        nubOrd $
          map (toSchemaHaskellModuleName apiName) allReferencedSchemas
            ++ foldMap (map (toResponseHaskellModuleName apiName) . operationResponseDependencies) operations
            ++ [ apiHaskellModuleName apiName,
                 responseHaskellModuleName apiName
               ]

      path = cabalFileName packageName
  write path (codegenCabalFile packageName allReferencedModules)
