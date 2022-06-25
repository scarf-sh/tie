{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Petstore.API.Api where

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

import Petstore.API.Request
import Petstore.API.Response



import Petstore.API.Schemas.Error
import Petstore.API.Schemas.Pet
import Petstore.API.Schemas.Pets

import Petstore.API.Response.CreatePets
import Petstore.API.Response.ListPets
import Petstore.API.Response.ShowPetById

data Api m = Api {
    -- | Create a pet
    createPets ::
        m CreatePetsResponse,
    -- | List all pets
    listPets ::
        -- @limit@ How many items to return at one time (max 100)
        (Data.Maybe.Maybe (GHC.Int.Int32)) ->
        m ListPetsResponse,
    -- | Info for a specific pet
    showPetById ::
        -- @petId@ The id of the pet to retrieve
        Data.Text.Text ->
        m ShowPetByIdResponse
}

application :: (Control.Monad.IO.Class.MonadIO m) => (forall a . Network.Wai.Request -> m a -> IO a) -> Api m -> Network.Wai.Application -> Network.Wai.Application
application run api notFound request respond =
    case Network.Wai.pathInfo request of
        ["pets"] ->
            case Network.Wai.requestMethod request of
                "GET" ->
                    optionalQueryParameter "limit" False (\__limit request respond ->
                        run request (do
                            response <- listPets api __limit
                            Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                        )) request respond
                "POST" ->
                    run request (do
                        response <- createPets api
                        Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                    )
                x ->
                    unsupportedMethod x

        ["pets", __petId] ->
            pathVariable __petId (\__petId request respond ->
                case Network.Wai.requestMethod request of
                    "GET" ->
                        run request (do
                            response <- showPetById api __petId
                            Control.Monad.IO.Class.liftIO (respond $! (toResponse response))
                        )
                    x ->
                        unsupportedMethod x) request respond

        _ ->
            notFound request respond
    where
        unsupportedMethod _ =
            respond (Network.Wai.responseBuilder (toEnum 405) [] mempty)
{-# INLINABLE application #-}