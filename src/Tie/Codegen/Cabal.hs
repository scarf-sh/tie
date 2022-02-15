{-# LANGUAGE OverloadedStrings #-}

module Tie.Codegen.Cabal (codegenCabalFile) where

import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP

codegenCabalFile :: Text -> [Text] -> Doc ann
codegenCabalFile packageName exposedModules =
  PP.vsep
    [ "cabal-version:" <+> "3.0",
      "name:" <+> PP.pretty packageName,
      "version:" <+> "0.1.0.0",
      "library" <> PP.line
        <> PP.indent
          2
          ( PP.vsep
              [ "build-depends:" <> PP.line
                  <> PP.indent
                    2
                    ( PP.vsep
                        [ "," <+> "aeson",
                          "," <+> "attoparsec",
                          "," <+> "base",
                          "," <+> "ghc-prim",
                          "," <+> "http-api-data",
                          "," <+> "http-types",
                          "," <+> "text",
                          "," <+> "wai"
                        ]
                    ),
                "exposed-modules:" <> PP.line
                  <> PP.indent
                    2
                    ( PP.vsep
                        (map PP.pretty exposedModules)
                    )
              ]
          )
    ]
