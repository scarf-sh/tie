{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Petstore.API.Schemas.Error where

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





data Error = Error
    {
        code :: GHC.Int.Int32,
        message :: Data.Text.Text
    }
    deriving (Show)

instance Data.Aeson.ToJSON Error where
    toJSON Error {..} = Data.Aeson.object
        [
            "code" Data.Aeson..= code,
            "message" Data.Aeson..= message
        ]

    toEncoding Error {..} = Data.Aeson.Encoding.pairs
        ( Data.Aeson.Encoding.pair "code" (Data.Aeson.toEncoding code) <>
          Data.Aeson.Encoding.pair "message" (Data.Aeson.toEncoding message)
        )

instance Data.Aeson.FromJSON Error where
    parseJSON = Data.Aeson.withObject "Error" $ \o ->
        Error
            <$> o Data.Aeson..: "code"
            <*> o Data.Aeson..: "message"