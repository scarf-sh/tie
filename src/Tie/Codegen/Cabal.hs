{-# LANGUAGE OverloadedStrings #-}

module Tie.Codegen.Cabal (codegenCabalFile) where

import qualified Data.Set as Set
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP

codegenCabalFile :: Text -> [Text] -> [Text] -> Doc ann
codegenCabalFile packageName exposedModules extraPackages =
  let packages =
        Set.toList . Set.fromList $
          [ "aeson",
            "attoparsec",
            "base",
            "bytestring",
            "ghc-prim",
            "http-api-data",
            "http-types",
            "text",
            "time",
            "wai"
          ]
            ++ extraPackages
   in PP.vsep
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
                        ( PP.vsep (map (\x -> "," <+> PP.pretty x) packages)
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
