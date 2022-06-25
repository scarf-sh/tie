{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Control.Concurrent.MVar
import Control.Exception (onException)
import Control.Monad.State.Strict (StateT, get, put, runStateT)
import Data.Int (Int32)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.RequestLogger
import qualified Petstore.API.Api as Petstore
import qualified Petstore.API.Response.CreatePets as Petstore
import qualified Petstore.API.Response.ListPets as Petstore
import qualified Petstore.API.Response.ShowPetById as Petstore
import qualified Petstore.API.Schemas.Error as Petstore
import qualified Petstore.API.Schemas.Pet as Petstore
import qualified Petstore.API.Schemas.Pets as Petstore

main :: IO ()
main = do
  -- This tiny example stores the application state in one, central MVar
  petsRef <- Control.Concurrent.MVar.newMVar []
  let port = 8080
  putStrLn ("Running Petstore on port " <> show port)
  Network.Wai.Handler.Warp.run port $
    Network.Wai.Middleware.RequestLogger.logStdoutDev $
      Petstore.application
        (runHandler petsRef)
        petstore
        -- In case the route can not be found in the petstore, we return
        -- a 404 error
        ( \_request respond ->
            respond (Network.Wai.responseLBS (toEnum 404) [] "Not found")
        )

-- | Runs a handler into IO
runHandler ::
  -- | Our global state MVar
  Control.Concurrent.MVar.MVar Petstore.Pets ->
  -- | The Wai request that matched the handler
  Network.Wai.Request ->
  -- | The handler itself
  StateT Petstore.Pets IO a ->
  IO a
runHandler stateRef _request action = do
  state <- Control.Concurrent.MVar.takeMVar stateRef
  (result, newState) <-
    runStateT action state
      `onException` Control.Concurrent.MVar.putMVar stateRef state
  Control.Concurrent.MVar.putMVar stateRef newState
  pure result

-- | The API implementation.
petstore :: Petstore.Api (StateT Petstore.Pets IO)
petstore =
  Petstore.Api {createPets, listPets, showPetById}

createPets :: StateT Petstore.Pets IO Petstore.CreatePetsResponse
createPets = do
  -- Set a few hardcoded pets to the state
  put
    [ Petstore.Pet
        { id = 1,
          name = "Mila",
          tag = Just "🐶"
        },
      Petstore.Pet
        { id = 2,
          name = "바다 거북",
          tag = Just "🐢"
        },
      Petstore.Pet
        { id = 3,
          name = "Edda",
          tag = Just "🐶"
        }
    ]
  pure Petstore.CreatePetsResponse201

listPets :: Maybe Int32 -> StateT Petstore.Pets IO Petstore.ListPetsResponse
listPets limit = do
  allPets <- get
  let pets = take (maybe maxBound fromIntegral limit) allPets
  pure (Petstore.ListPetsResponse200 pets Nothing)

showPetById :: Text -> StateT Petstore.Pets IO Petstore.ShowPetByIdResponse
showPetById petId = do
  allPets <- get
  case find
    (\Petstore.Pet {id} -> Text.pack (show id) == petId)
    allPets of
    Nothing ->
      pure $
        Petstore.ShowPetByIdDefaultResponse
          (toEnum 404)
          ( Petstore.Error
              { code = 1,
                message = "Pet not found"
              }
          )
    Just pet ->
      pure $
        Petstore.ShowPetByIdResponse200 pet
