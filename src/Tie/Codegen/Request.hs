{-# LANGUAGE OverloadedStrings #-}

module Tie.Codegen.Request (codegenRequestAuxFile) where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Paths_tie (getDataFileName)
import Prettyprinter (Doc, hsep, vsep)
import qualified Prettyprinter.Util as Prettyprinter
import System.IO.Unsafe (unsafePerformIO)
import Tie.Name (Name)

auxTemplate :: Text
auxTemplate = unsafePerformIO $ do
  file <- getDataFileName "Request.template.hs"
  contents <- ByteString.readFile file
  pure (decodeUtf8 contents)
{-# NOINLINE auxTemplate #-}

codegenRequestAuxFile ::
  -- | Module name
  Text ->
  Text
codegenRequestAuxFile moduleName =
  Text.replace "Tie.Template.Request_" moduleName auxTemplate
