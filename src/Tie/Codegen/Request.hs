{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Tie.Codegen.Request (codegenRequestAuxFile) where

import qualified Data.ByteString as ByteString
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import qualified Data.Text as Text
import Prettyprinter (Doc, hsep, vsep)
import qualified Prettyprinter.Util as Prettyprinter
import System.IO.Unsafe (unsafePerformIO)
import Tie.Name (Name)

templateContents :: ByteString
templateContents = $(embedStringFile =<< makeRelativeToProject "Request.template.hs")

auxTemplate :: Text
auxTemplate = decodeUtf8 templateContents
{-# NOINLINE auxTemplate #-}

codegenRequestAuxFile ::
  -- | Module name
  Text ->
  Text
codegenRequestAuxFile moduleName =
  Text.replace "Tie.Template.Request_" moduleName auxTemplate
