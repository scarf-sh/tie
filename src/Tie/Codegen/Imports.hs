{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Codegen.Imports
  ( codegenModuleHeader,
    codegenSchemaDependencies,
    codegenResponseDependencies,
    codegenExtraApiModuleDependencies,
    codegenExtraResponseModuleDependencies,
    codegenExternalHaskellDependencies,
  )
where

import Prettyprinter (Doc, vsep, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import Tie.Name
  ( ApiName,
    Name,
    requestHaskellModuleName,
    responseHaskellModuleName,
    toResponseHaskellModuleName,
    toSchemaHaskellModuleName,
  )

codegenModuleHeader :: Text -> Doc ann
codegenModuleHeader moduleName =
  let languageExtensions :: [Text]
      languageExtensions =
        sort
          [ "BangPatterns",
            "DataKinds",
            "DuplicateRecordFields",
            "OverloadedStrings",
            "ScopedTypeVariables",
            "RankNTypes",
            "RecordWildCards"
          ]

      imports :: [Text]
      imports =
        sort
          [ "Control.Applicative",
            "Control.Exception",
            "Control.Monad",
            "Control.Monad.IO.Class",
            "Data.Aeson",
            "Data.Aeson.Types",
            "Data.Aeson.Encoding",
            "Data.Attoparsec.ByteString",
            "Data.ByteString",
            "Data.List",
            "Data.List.NonEmpty",
            "Data.Map",
            "Data.Maybe",
            "Data.Text",
            "Data.Time",
            "Data.Text.Encoding",
            "GHC.Float",
            "GHC.Int",
            "GHC.Records",
            "GHC.Types",
            "Network.HTTP.Types",
            "Network.Wai",
            "Web.HttpApiData"
          ]
   in PP.vcat $
        ( map
            (\extension -> "{-#" <+> "LANGUAGE" <+> PP.pretty extension <+> "#-}")
            languageExtensions
        )
          ++ ["module" <+> PP.pretty moduleName <+> "where", ""]
          ++ ( map
                 (\import_ -> "import" <+> "qualified" <+> PP.pretty import_)
                 imports
             )

codegenExtraApiModuleDependencies :: ApiName -> Doc ann
codegenExtraApiModuleDependencies apiName =
  vsep
    [ "import" <+> PP.pretty (requestHaskellModuleName apiName),
      "import" <+> PP.pretty (responseHaskellModuleName apiName)
    ]

codegenExtraResponseModuleDependencies :: ApiName -> Doc ann
codegenExtraResponseModuleDependencies apiName =
  "import" <+> PP.pretty (responseHaskellModuleName apiName)

codegenSchemaDependencies :: ApiName -> [Name] -> Doc ann
codegenSchemaDependencies apiName dependencies =
  PP.vsep
    [ "import" <+> PP.pretty (toSchemaHaskellModuleName apiName dependency)
      | dependency <- dependencies
    ]

codegenResponseDependencies :: ApiName -> [Name] -> Doc ann
codegenResponseDependencies apiName dependencies =
  PP.vsep
    [ "import" <+> PP.pretty (toResponseHaskellModuleName apiName dependency)
      | dependency <- dependencies
    ]

codegenExternalHaskellDependencies :: [Text] -> Doc ann
codegenExternalHaskellDependencies dependencies =
  PP.vsep
    [ "import" <+> "qualified" <+> PP.pretty dependency
      | dependency <- dependencies
    ]
