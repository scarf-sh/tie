{-# LANGUAGE OverloadedStrings #-}

module Test.Tie.Golden (test_Golden_tests) where

import Data.ByteString.Builder (toLazyByteString)
import Paths_tie (getDataDir)
import System.FilePath (normalise, replaceExtension, (</>))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Tie (generate, withTestWriter)

test_Golden_tests :: IO [TestTree]
test_Golden_tests = do
  dataDir <- getDataDir
  inputs <- findByExtension [".yaml"] (dataDir </> "test" </> "golden")
  pure
    [ goldenVsStringDiff
        ("Test " <> input)
        (\ref new -> ["diff", "-u", ref, new])
        (replaceExtension input ".yaml.out")
        ( do
            (_, output) <- withTestWriter $ \writer ->
              generate
                writer
                "test" -- package name
                "Test" -- module name
                input
            pure (toLazyByteString output)
        )
      | input' <- inputs,
        let input = normalise input'
    ]
