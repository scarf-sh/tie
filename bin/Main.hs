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
    infoOption,
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
import Data.Version (showVersion)
import Paths_tie (version)

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
          <> help "The directory output"
      )
    <*> option
      str
      ( long "module-name"
          <> metavar "MODULE"
          <> showDefault
          <> value "OpenAPI"
          <> help "Name of the generated top level module"
      )
    <*> option
      str
      ( long "package-name"
          <> metavar "PACKAGE"
          <> showDefault
          <> value "open-api"
          <> help "Name of the generated cabal project"
      )
    <*> many
      ( option
          str
          ( long "extra-package"
              <> metavar "PACKAGE"
              <> help "Extra packages to include in the generated cabal project"
          )
      )
    <*> strArgument
      ( metavar "FILE"
          <> help "OpenAPI specification file"
      )

versioner :: Parser (a -> a)
versioner = infoOption ("tie " <> showVersion version)
  ( long "version"
  <> help "Print Tie version"
  )

main :: IO ()
main = do
  Input {..} <-
    execParser $
      info
        (helper <*> versioner <*> options)
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
