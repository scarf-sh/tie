{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Operation
  ( StatusCode,
    Param (..),
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
  )
where

import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as Text
import Tie.Name (Name, fromText)
import Tie.Resolve (Resolver, resolve)
import Tie.Type
  ( Named,
    Type,
    isBasicType,
    namedType,
    namedTypeDependencies,
    schemaRefToType,
  )
import Prelude hiding (Type)

-- | HTTP Status code type
type StatusCode = Int

-- | Request body descriptor
data RequestBody = RequestBody
  { jsonRequestBodyContent :: Named Type
  }

-- | Response descriptor
data Response = Response
  { -- | JSON schema of the response
    jsonResponseContent :: Named Type
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

data Param = Param
  { name :: Name,
    paramIn :: ParamIn,
    schema :: Named Type,
    required :: Bool
  }

-- | Internal representation for an 'OpenApi.Operation'.
data Operation = Operation
  { -- | Name of the operation. Used for identifiers.
    name :: Name,
    -- | Path
    path :: Path,
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
    unsupportedMediaType :: forall a. m a,
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
operationSchemaDependencies :: (Named Type -> [Name]) -> Operation -> [Name]
operationSchemaDependencies getDependencies Operation {..} =
  concat $
    [ getDependencies jsonRequestBodyContent
      | Just RequestBody {jsonRequestBodyContent} <- [requestBody]
    ]
      ++ [ getDependencies jsonResponseContent
           | Just Response {jsonResponseContent} <- [defaultResponse]
         ]
      ++ [ getDependencies jsonResponseContent
           | (_, Response {jsonResponseContent}) <- responses
         ]

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
  operationId <-
    whenNothing _operationOperationId missingOperationId
  path <-
    pathToPath
      resolver
      errors
      path
      -- Operations override pathItem params
      (_operationParameters ++ params)
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
      unsupportedMediaType
  referencedSchema <-
    whenNothing
      _mediaTypeObjectSchema
      requestBodyMissingSchema
  type_ <-
    schemaRefToType resolver referencedSchema
  pure
    RequestBody
      { jsonRequestBodyContent = type_
      }

responseToResponse ::
  Monad m =>
  Resolver m ->
  Errors m ->
  OpenApi.Response ->
  m Response
responseToResponse resolver Errors {..} response = do
  OpenApi.MediaTypeObject {..} <-
    whenNothing
      (InsOrd.lookup "application/json" (OpenApi._responseContent response))
      unsupportedMediaType
  -- TODO take care of headers
  referencedSchema <-
    whenNothing
      _mediaTypeObjectSchema
      requestBodyMissingSchema
  type_ <-
    schemaRefToType resolver referencedSchema
  pure
    Response
      { jsonResponseContent = type_
      }

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
        paramIn = case _paramIn of
          OpenApi.ParamQuery -> InQuery
          OpenApi.ParamHeader -> InHeader
          OpenApi.ParamPath -> InPath
          OpenApi.ParamCookie -> InCookie,
        required = fromMaybe False _paramRequired,
        schema = typ
      }

pathToPath ::
  Monad m =>
  Resolver m ->
  Errors m ->
  -- | URL Path
  FilePath ->
  -- | Available 'OpenApi.Param's
  [OpenApi.Referenced OpenApi.Param] ->
  m Path
pathToPath resolver errors@Errors {..} textualPath availableParams = do
  let path = parsePath textualPath
  params <- traverse (resolve resolver) availableParams
  forM path $ \segment ->
    forM segment $ \paramName -> do
      param <-
        whenNothing
          ( find
              (\x -> OpenApi._paramName x == paramName)
              params
          )
          (unknownParameter paramName)
      param <- paramToParam resolver errors param
      when
        (paramIn param /= InPath)
        paramNotInPath
      _ <-
        whenNothing
          (isBasicType (namedType (schema param)))
          paramNotBasicType
      pure param
