{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Tie.Operation
  ( StatusCode,
    Param (..),
    Header (..),
    RequestBody (..),
    Response (..),
    Operation (..),
    pathItemsToOperation,
    operationToOperation,

    -- * Errors
    Errors (..),
    errors,

    -- * Path
    Path,
    PathSegment (..),
    parsePath,

    -- * Dependencies
    operationSchemaDependencies,
    operationResponseDependencies,
    operationExternalDependencies,

    -- * Normalization
    normalizeOperation,
  )
where

import Control.Monad.Writer (WriterT (..), runWriterT)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as Text
import Tie.Name
  ( Name,
    apiDefaultResponseConstructorName,
    apiResponseConstructorName,
    fromText,
    operationParamTypeName,
    operationRequestBodyName,
  )
import Tie.Resolve (Resolver, resolve)
import Tie.Type
  ( Named,
    Type,
    isBasicType,
    namedType,
    namedTypeDependencies,
    normalizeNamedType,
    schemaRefToType,
    typeExternalDependencies,
  )
import Prelude hiding (Type)

-- | HTTP Status code type
type StatusCode = Int

-- | Request body descriptor
data RequestBody = RequestBody
  { description :: Maybe Text,
    jsonRequestBodyContent :: Named Type
  }

data Header = Header
  { name :: Name,
    description :: Maybe Text,
    schema :: Maybe (Named Type),
    required :: Bool
  }

-- | Response descriptor
data Response = Response
  { description :: Text,
    -- | JSON schema of the response
    jsonResponseContent :: Maybe (Named Type),
    -- | Response headers
    headers :: [Header]
  }

-- | Internal representation of a path.
data PathSegment variable
  = StaticSegment Text
  | VariableSegment variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Internal representation of a Path
type Path = [PathSegment Param]

data ParamIn
  = InPath
  | InQuery
  | InHeader
  | InCookie
  deriving (Eq, Ord, Show)

-- | 'Param' corresponds to OpenAPI's Parameter component.
data Param = Param
  { name :: Name,
    summary :: Maybe Text,
    paramIn :: ParamIn,
    schema :: Named Type,
    required :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Internal representation for an 'OpenApi.Operation'.
data Operation = Operation
  { -- | Name of the operation. Used for identifiers.
    name :: Name,
    -- | A short summary of what the operation is.
    summary :: Maybe Text,
    -- | Path
    path :: Path,
    -- Query parameters
    queryParams :: [Param],
    -- | Header parameters
    headerParams :: [Param],
    -- | HTTP method for this operation (Get, Post, Put, Delete)
    method :: Text,
    -- | Type of the request body (if any) for this 'Operation'.
    requestBody :: Maybe RequestBody,
    -- | Default response.
    defaultResponse :: Maybe Response,
    -- | Responses
    responses :: [(StatusCode, Response)]
  }

data Errors m = Errors
  { missingOperationId :: forall a. m a,
    unsupportedMediaType :: forall a. HasCallStack => m a,
    requestBodyMissingSchema :: forall a. m a,
    unknownParameter :: forall a. Text -> m a,
    paramMissingSchema :: forall a. m a,
    paramNotInPath :: forall a. m a,
    paramNotBasicType :: forall a. m a
  }

errors :: Errors m
errors =
  Errors
    { missingOperationId =
        error "missing operation id",
      unsupportedMediaType =
        error "unsupported media type",
      requestBodyMissingSchema =
        error "request body missing media type",
      unknownParameter =
        error "unknown parameter",
      paramMissingSchema =
        error "param missing schema",
      paramNotInPath =
        error "param not 'in path' type",
      paramNotBasicType =
        error "only basic types are supported for parameters"
    }

-- | Returns the dependencies on schemas of an operation. Parameterized so
-- that shallow vs. all transitive dependencies can be extracted.
operationSchemaDependencies :: (Named Type -> [a]) -> Operation -> [a]
operationSchemaDependencies getDependencies Operation {..} =
  concat $
    [ getDependencies jsonRequestBodyContent
      | Just RequestBody {jsonRequestBodyContent} <- [requestBody]
    ]
      ++ map getDependencies (pathDependencies path)
      ++ [ getDependencies jsonContent
           | Just Response {jsonResponseContent = Just jsonContent} <- [defaultResponse]
         ]
      ++ [ getDependencies jsonContent
           | (_, Response {jsonResponseContent = Just jsonContent}) <- responses
         ]
      ++ [ getDependencies schema
           | Param {schema} <- queryParams
         ]
      ++ [ getDependencies schema
           | Param {schema} <- headerParams
         ]

operationExternalDependencies :: Operation -> [Text]
operationExternalDependencies =
  operationSchemaDependencies (typeExternalDependencies . namedType)

-- | Dependencies in the Response.* modules.
operationResponseDependencies :: Operation -> [Name]
operationResponseDependencies Operation {name} = [name]

pathItemsToOperation ::
  Monad m =>
  Resolver m ->
  -- | Conversion error cases
  Errors m ->
  -- | URLs
  [(FilePath, OpenApi.PathItem)] ->
  m [Operation]
pathItemsToOperation resolver errors@Errors {..} pathInfos = do
  items <- forM pathInfos $ \(path, OpenApi.PathItem {..}) -> do
    get <-
      forM _pathItemGet (operationToOperation resolver errors "GET" path _pathItemParameters)
    put <-
      forM _pathItemPut (operationToOperation resolver errors "PUT" path _pathItemParameters)
    post <-
      forM _pathItemPost (operationToOperation resolver errors "POST" path _pathItemParameters)
    delete <-
      forM _pathItemDelete (operationToOperation resolver errors "DELETE" path _pathItemParameters)
    options <-
      forM _pathItemOptions (operationToOperation resolver errors "OPTIONS" path _pathItemParameters)
    head <-
      forM _pathItemHead (operationToOperation resolver errors "HEAD" path _pathItemParameters)
    patch <-
      forM _pathItemPatch (operationToOperation resolver errors "PATCH" path _pathItemParameters)
    trace <-
      forM _pathItemTrace (operationToOperation resolver errors "TRACE" path _pathItemParameters)
    pure (catMaybes [get, put, post, delete, options, head, patch, trace])
  pure (concat items)

-- TODO name
operationToOperation ::
  Monad m =>
  Resolver m ->
  -- | Conversion error cases
  Errors m ->
  -- | HTTP Method
  Text ->
  -- | Path
  FilePath ->
  -- | Params defined at the PathItem level
  [OpenApi.Referenced OpenApi.Param] ->
  OpenApi.Operation ->
  m Operation
operationToOperation resolver errors@Errors {..} method path params OpenApi.Operation {..} = do
  -- Operations override pathItem params
  allParams <- traverse (resolve resolver) (_operationParameters ++ params)

  let pathParams =
        [ param
          | param@OpenApi.Param {_paramIn} <- allParams,
            _paramIn == OpenApi.ParamPath
        ]

      queryParams =
        [ param
          | param@OpenApi.Param {_paramIn} <- allParams,
            _paramIn == OpenApi.ParamQuery
        ]

      headerParams =
        [ param
          | param@OpenApi.Param {_paramIn} <- allParams,
            _paramIn == OpenApi.ParamHeader
        ]

  operationId <-
    whenNothing _operationOperationId missingOperationId
  path <-
    pathToPath
      resolver
      errors
      path
      allParams
  queryParams <-
    traverse (paramToParam resolver errors) queryParams
  headerParams <-
    traverse (paramToParam resolver errors) headerParams
  requestBody <- forM _operationRequestBody $ \referencedRequestBody -> do
    requestBody <- resolve resolver referencedRequestBody
    requestBodyToRequestBody resolver errors requestBody
  defaultResponse <- forM (OpenApi._responsesDefault _operationResponses) $ \referencedResponse -> do
    response <- resolve resolver referencedResponse
    responseToResponse resolver errors response
  responses <- forM (InsOrd.toList (OpenApi._responsesResponses _operationResponses)) $ \(statusCode, referencedResponse) -> do
    response <- resolve resolver referencedResponse
    (,) <$> pure statusCode <*> responseToResponse resolver errors response
  pure
    Operation
      { name = fromText operationId,
        summary = _operationSummary,
        responses = sortOn fst responses,
        ..
      }

requestBodyToRequestBody ::
  Monad m =>
  Resolver m ->
  Errors m ->
  OpenApi.RequestBody ->
  m RequestBody
requestBodyToRequestBody resolver Errors {..} requestBody = do
  -- TODO support form inputs as well
  OpenApi.MediaTypeObject {..} <-
    whenNothing
      (InsOrd.lookup "application/json" (OpenApi._requestBodyContent requestBody))
      (traceShow requestBody $ unsupportedMediaType)
  referencedSchema <-
    whenNothing
      _mediaTypeObjectSchema
      requestBodyMissingSchema
  type_ <-
    schemaRefToType resolver referencedSchema
  pure
    RequestBody
      { description = OpenApi._requestBodyDescription requestBody,
        jsonRequestBodyContent = type_
      }

responseToResponse ::
  Monad m =>
  Resolver m ->
  Errors m ->
  OpenApi.Response ->
  m Response
responseToResponse resolver errors@Errors {..} response@OpenApi.Response {..} = do
  responseTy <- responseMediaTypeObject resolver errors response
  headers <- traverse (uncurry (headerToHeader resolver errors)) (InsOrd.toList _responseHeaders)
  pure
    Response
      { description = OpenApi._responseDescription response,
        jsonResponseContent = responseTy,
        headers
      }

responseMediaTypeObject :: Monad m => Resolver m -> Errors m -> OpenApi.Response -> m (Maybe (Named Type))
responseMediaTypeObject resolver Errors {..} response
  | Just OpenApi.MediaTypeObject {..} <- InsOrd.lookup "application/json" (OpenApi._responseContent response) = do
    referencedSchema <-
      whenNothing
        _mediaTypeObjectSchema
        requestBodyMissingSchema
    type_ <-
      schemaRefToType resolver referencedSchema
    pure (Just type_)
  | InsOrd.null (OpenApi._responseContent response) = do
    pure Nothing
  | otherwise =
    traceShow response $ unsupportedMediaType

parsePath :: FilePath -> [PathSegment Text]
parsePath path =
  let toPathSegment s
        | Just s <- Text.stripPrefix "{" s,
          Just s <- Text.stripSuffix "}" s =
          VariableSegment s
        | otherwise =
          StaticSegment s
   in case Text.splitOn "/" (toText path) of
        -- leading / results in a leading empty string after split
        "" : segments ->
          map toPathSegment segments
        segments ->
          -- TODO this is probably an error
          map toPathSegment segments

pathDependencies :: Path -> [Named Type]
pathDependencies path =
  [schema | VariableSegment Param {schema} <- path]

paramToParam ::
  Monad m =>
  Resolver m ->
  Errors m ->
  OpenApi.Param ->
  m Param
paramToParam resolver Errors {..} OpenApi.Param {..} = do
  schema <- whenNothing _paramSchema paramMissingSchema
  typ <- schemaRefToType resolver schema
  pure
    Param
      { name = fromText _paramName,
        summary = _paramDescription,
        paramIn = case _paramIn of
          OpenApi.ParamQuery -> InQuery
          OpenApi.ParamHeader -> InHeader
          OpenApi.ParamPath -> InPath
          OpenApi.ParamCookie -> InCookie,
        required = fromMaybe False _paramRequired,
        schema = typ
      }

headerToHeader ::
  Monad m =>
  Resolver m ->
  Errors m ->
  Text ->
  OpenApi.Referenced OpenApi.Header ->
  m Header
headerToHeader resolver Errors {..} name referencedHeader = do
  OpenApi.Header {..} <- resolve resolver referencedHeader
  schema <- traverse (schemaRefToType resolver) _headerSchema
  pure
    Header
      { schema,
        name = fromText name,
        description = _headerDescription,
        required = fromMaybe False _headerRequired
      }

pathToPath ::
  Monad m =>
  Resolver m ->
  Errors m ->
  -- | URL Path
  FilePath ->
  -- | Available 'OpenApi.Param's
  [OpenApi.Param] ->
  m Path
pathToPath resolver errors@Errors {..} textualPath params = do
  let path = parsePath textualPath
  forM path $ \segment ->
    forM segment $ \paramName -> do
      param <-
        whenNothing
          ( find
              (\x -> OpenApi._paramName x == paramName)
              params
          )
          (unknownParameter paramName)
      param@Param {schema} <- paramToParam resolver errors param
      when
        (paramIn param /= InPath)
        paramNotInPath
      _ <-
        whenNothing
          (isBasicType (namedType schema))
          paramNotBasicType
      pure param

normalizeParam :: Monad m => Name -> Param -> m (Param, [(Name, Type)])
normalizeParam operationName param@Param {..} = do
  (normedType, inlineDefinitions) <-
    normalizeNamedType
      (pure (operationParamTypeName operationName name))
      schema
  pure (param {schema = normedType} :: Param, inlineDefinitions)

normalizeResponse :: Monad m => Name -> Response -> m (Response, [(Name, Type)])
normalizeResponse name response@Response {..} =
  case jsonResponseContent of
    Nothing -> pure (response, [])
    Just jsonContent -> do
      (normedType, inlineDefinitions) <- normalizeNamedType (pure name) jsonContent
      pure (response {jsonResponseContent = Just normedType}, inlineDefinitions)

normalizeRequestBody :: Monad m => Name -> RequestBody -> m (RequestBody, [(Name, Type)])
normalizeRequestBody name body@RequestBody {..} = do
  (normedType, inlineDefinitions) <-
    normalizeNamedType
      (pure (operationRequestBodyName name))
      jsonRequestBodyContent
  pure (body {jsonRequestBodyContent = normedType}, inlineDefinitions)

normalizeOperation :: Monad m => Operation -> m (Operation, [(Name, Type)])
normalizeOperation operation@Operation {..} =
  fmap (second (sortOn fst)) $
    runWriterT $ do
      queryParams <-
        traverse
          (WriterT . normalizeParam name)
          queryParams
      headerParams <-
        traverse
          (WriterT . normalizeParam name)
          headerParams
      requestBody <-
        traverse
          (WriterT . normalizeRequestBody name)
          requestBody
      defaultResponse <-
        traverse
          (WriterT . normalizeResponse (apiDefaultResponseConstructorName name))
          defaultResponse
      responses <-
        traverse
          ( \(status, response) ->
              ( (status,)
                  <$> WriterT
                    (normalizeResponse (apiResponseConstructorName name status) response)
              )
          )
          responses
      pure Operation {..}
