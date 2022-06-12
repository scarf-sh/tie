module Tie.Template.Response_
  ( ToResponse (..),
  )
where

import qualified Network.Wai

class ToResponse a where
  toResponse :: a -> Network.Wai.Response
