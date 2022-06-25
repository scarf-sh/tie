module Petstore.API.Response
  ( ToResponse (..),
  )
where

import qualified Network.Wai

class ToResponse a where
  toResponse :: a -> Network.Wai.Response
