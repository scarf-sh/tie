{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Petstore.API.Schemas.Pet where

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





data Pet = Pet
    {
        id :: GHC.Int.Int64,
        name :: Data.Text.Text,
        tag :: (Data.Maybe.Maybe (Data.Text.Text))
    }
    deriving (Show)

instance Data.Aeson.ToJSON Pet where
    toJSON Pet {..} = Data.Aeson.object
        [
            "id" Data.Aeson..= id,
            "name" Data.Aeson..= name,
            "tag" Data.Aeson..= tag
        ]

    toEncoding Pet {..} = Data.Aeson.Encoding.pairs
        ( Data.Aeson.Encoding.pair "id" (Data.Aeson.toEncoding id) <>
          Data.Aeson.Encoding.pair "name" (Data.Aeson.toEncoding name) <>
          Data.Aeson.Encoding.pair "tag" (Data.Aeson.toEncoding tag)
        )

instance Data.Aeson.FromJSON Pet where
    parseJSON = Data.Aeson.withObject "Pet" $ \o ->
        Pet
            <$> o Data.Aeson..: "id"
            <*> o Data.Aeson..: "name"
            <*> o Data.Aeson..:? "tag"