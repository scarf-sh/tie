{-# LANGUAGE OverloadedStrings #-}

module Tie.Template.Request_
  ( pathVariable,
    requiredQueryParameter,
    optionalQueryParameter,
    requiredHeader,
    optionalHeader,
    parseRequestBodyJSON,
  )
where

import Data.Aeson (FromJSON, parseJSON)
import qualified Data.Aeson.Parser
import qualified Data.Aeson.Types
import Data.Attoparsec.ByteString (eitherResult, parseWith)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Types (HeaderName, hContentType)
import qualified Network.Wai as Wai
import Web.HttpApiData
  ( FromHttpApiData,
    parseHeader,
    parseQueryParam,
    parseUrlPiece,
  )

pathVariable ::
  FromHttpApiData a =>
  -- | Path variable value
  Text ->
  (a -> Wai.Application) ->
  Wai.Application
pathVariable value withVariable = \request respond ->
  case parseUrlPiece value of
    Left _err ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Right x ->
      withVariable x request respond
{-# INLINEABLE pathVariable #-}

requiredQueryParameter ::
  FromHttpApiData a =>
  ByteString ->
  (a -> Wai.Application) ->
  Wai.Application
requiredQueryParameter name withParam = \request respond ->
  case List.lookup name (Wai.queryString request) of
    Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just (Just value) ->
      case parseQueryParam (Text.decodeUtf8 value) of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withParam x request respond
{-# INLINEABLE requiredQueryParameter #-}

optionalQueryParameter ::
  FromHttpApiData a =>
  ByteString ->
  -- | Allow empty, e.g. "x="
  Bool ->
  (Maybe a -> Wai.Application) ->
  Wai.Application
optionalQueryParameter name allowEmpty withParam = \request respond ->
  case List.lookup name (Wai.queryString request) of
    Nothing ->
      withParam Nothing request respond
    Just Nothing
      | allowEmpty ->
        withParam Nothing request respond
      | otherwise ->
        respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just (Just value) ->
      case parseQueryParam (Text.decodeUtf8 value) of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withParam (Just x) request respond
{-# INLINEABLE optionalQueryParameter #-}

optionalHeader ::
  FromHttpApiData a =>
  HeaderName ->
  (Maybe a -> Wai.Application) ->
  Wai.Application
optionalHeader name withHeader = \request respond ->
  case List.lookup name (Wai.requestHeaders request) of
    Nothing ->
      withHeader Nothing request respond
    Just value ->
      case parseHeader value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withHeader (Just x) request respond
{-# INLINEABLE optionalHeader #-}

requiredHeader ::
  FromHttpApiData a =>
  HeaderName ->
  (a -> Wai.Application) ->
  Wai.Application
requiredHeader name withHeader = \request respond ->
  case List.lookup name (Wai.requestHeaders request) of
    Nothing ->
      respond (Wai.responseBuilder (toEnum 400) [] mempty)
    Just value ->
      case parseHeader value of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right x ->
          withHeader x request respond
{-# INLINEABLE requiredHeader #-}

parseRequestBodyJSON :: FromJSON a => (a -> Wai.Application) -> Wai.Application
parseRequestBodyJSON withBody = \request respond ->
  case List.lookup hContentType (Wai.requestHeaders request) of
    Just "application/json" -> do
      result <- parseWith (Wai.getRequestBodyChunk request) Data.Aeson.Parser.json' mempty
      case eitherResult result of
        Left _err ->
          respond (Wai.responseBuilder (toEnum 400) [] mempty)
        Right value ->
          case Data.Aeson.Types.parseEither Data.Aeson.parseJSON value of
            Left _err ->
              respond (Wai.responseBuilder (toEnum 400) [] mempty)
            Right body ->
              withBody body request respond
    _ ->
      respond (Wai.responseBuilder (toEnum 415) [] mempty)
{-# INLINEABLE parseRequestBodyJSON #-}
