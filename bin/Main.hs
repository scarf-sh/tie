{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Options.Applicative
  ( Parser,
    auto,
    execParser,
    flag,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    showDefault,
    str,
    strArgument,
    strOption,
    switch,
    value,
  )
import System.Environment (getArgs)
import Tie (fileWriter, generate)
import Prelude hiding (Option)

data Input = Input
  { outputDirectory :: FilePath,
    moduleName :: Text,
    packageName :: Text,
    extraPackages :: [Text],
    inputFile :: FilePath
  }

options :: Parser Input
options =
  Input
    <$> option
      str
      ( long "output"
          <> short 'o'
          <> metavar "DIR"
          <> showDefault
          <> value "out"
      )
    <*> option
      str
      ( long "module-name"
          <> metavar "MODULE"
          <> showDefault
          <> value "OpenAPI"
      )
    <*> option
      str
      ( long "package-name"
          <> metavar "PACKAGE"
          <> showDefault
          <> value "open-api"
      )
    <*> many
      ( option
          str
          ( long "extra-package"
              <> metavar "PACKAGE"
          )
      )
    <*> strArgument
      ( metavar "FILE"
      )

main :: IO ()
main = do
  Input {..} <-
    execParser $
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Generate a Haskell server from an OpenAPI3 specification"
            <> header "tie - openapi3 server code generator"
        )
  generate
    (fileWriter outputDirectory)
    packageName
    moduleName
    extraPackages
    inputFile
