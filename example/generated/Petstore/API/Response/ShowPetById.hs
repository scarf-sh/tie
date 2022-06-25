{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Petstore.API.Response.ShowPetById where

import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Data.Aeson
import qualified Data.Aeson.Encoding
import qualified Data.Aeson.Parser
import qualified Data.Aeson.Types
import qualified Data.Attoparsec.ByteString
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Time
import qualified Data.Text.Encoding
import qualified GHC.Float
import qualified GHC.Int
import qualified GHC.Types
import qualified Network.HTTP.Types
import qualified Network.Wai
import qualified Web.HttpApiData

import Petstore.API.Schemas.Error
import Petstore.API.Schemas.Pet

import Petstore.API.Response

data ShowPetByIdResponse
    = ShowPetByIdResponse200 Pet
    | ShowPetByIdDefaultResponse Network.HTTP.Types.Status Error
    deriving (Show)

instance ToResponse ShowPetByIdResponse where
    toResponse (ShowPetByIdResponse200 x) =
        Network.Wai.responseBuilder (toEnum 200) ([(Network.HTTP.Types.hContentType, "application/json")]) (Data.Aeson.fromEncoding (Data.Aeson.toEncoding x))
    toResponse (ShowPetByIdDefaultResponse status x) =
        Network.Wai.responseBuilder status ([(Network.HTTP.Types.hContentType, "application/json")]) (Data.Aeson.fromEncoding (Data.Aeson.toEncoding x))