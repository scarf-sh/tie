{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Tie.Writer
  ( Writer,
    fileWriter,
    withTestWriter,
  )
where

import Data.ByteString.Builder (Builder, hPutBuilder)
import Data.Text.Lazy.Encoding (encodeUtf8Builder)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (WriteMode), withBinaryFile)

-- | Abstraction for storing generated code on disk.
type Writer m = forall ann. FilePath -> Doc ann -> m ()

-- | Renders a 'Doc' to a 'Builder' - ready to be written to disk.
-- TODO move somewhere else
render :: Doc ann -> Builder
render =
  encodeUtf8Builder . PP.renderLazy . PP.layoutPretty PP.defaultLayoutOptions

-- | Renders 'Doc's to a file just as you would expect. Writes files relative
-- to the given output directory.
fileWriter :: MonadIO m => FilePath -> Writer m
fileWriter outputDirectory path doc = liftIO $ do
  let fullPath = outputDirectory </> path
  createDirectoryIfMissing True (takeDirectory fullPath)
  withBinaryFile fullPath WriteMode $ \file ->
    Data.ByteString.Builder.hPutBuilder file (render doc)

-- | Collects all the FilePath and Doc pairs and returns them concatenated
-- in one output
withTestWriter :: MonadIO m => (Writer m -> m a) -> m (a, Builder)
withTestWriter action = do
  ref <- liftIO (newIORef [])
  result <- action $ \file doc ->
    liftIO (modifyIORef' ref ((file, PP.unAnnotate doc) :))
  docs <- liftIO (readIORef ref)
  pure (result, renderOneBigFile (sortOn fst docs))
  where
    renderOneBigFile docs =
      render $
        PP.concatWith
          (\x y -> x <> PP.line <> "---------------------" <> PP.line <> y)
          [ PP.vsep
              [ PP.pretty (toText file),
                mempty,
                doc
              ]
            | (file, doc) <- docs
          ]
