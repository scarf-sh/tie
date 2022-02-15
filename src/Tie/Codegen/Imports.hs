{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Codegen.Imports
  ( codegenModuleHeader,
    codegenSchemaDependencies,
    codegenResponseDependencies,
    codegenExtraApiModuleDependencies,
  )
where

import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import Tie.Name
  ( ApiName,
    Name,
    responseHaskellModuleName,
    toResponseHaskellModuleName,
    toSchemaHaskellModuleName,
  )

codegenModuleHeader :: Text -> Doc ann
codegenModuleHeader moduleName =
  "{-#" <+> "LANGUAGE" <+> "DuplicateRecordFields" <+> "#-}"
    <> PP.line
    <> "{-#" <+> "LANGUAGE" <+> "OverloadedStrings" <+> "#-}"
    <> PP.line
    <> "{-#" <+> "LANGUAGE" <+> "RankNTypes" <+> "#-}"
    <> PP.line
    <> "{-#" <+> "LANGUAGE" <+> "RecordWildCards" <+> "#-}"
    <> PP.line
    <> "module" <+> PP.pretty moduleName <+> "where"
    <> PP.line
    <> PP.line
    <> "import" <+> "qualified" <+> "Control.Applicative"
    <> PP.line
    <> "import" <+> "qualified" <+> "Control.Monad.IO.Class"
    <> PP.line
    <> "import" <+> "qualified" <+> "Data.Aeson"
    <> PP.line
    <> "import" <+> "qualified" <+> "Data.Aeson.Parser"
    <> PP.line
    <> "import" <+> "qualified" <+> "Data.Aeson.Types"
    <> PP.line
    <> "import" <+> "qualified" <+> "Data.Attoparsec.ByteString"
    <> PP.line
    <> "import" <+> "qualified" <+> "Data.Maybe"
    <> PP.line
    <> "import" <+> "qualified" <+> "Data.Text"
    <> PP.line
    <> "import" <+> "qualified" <+> "GHC.Types"
    <> PP.line
    <> "import" <+> "qualified" <+> "Network.HTTP.Types"
    <> PP.line
    <> "import" <+> "qualified" <+> "Network.Wai"
    <> PP.line
    <> "import" <+> "qualified" <+> "Web.HttpApiData"

codegenExtraApiModuleDependencies :: ApiName -> Doc ann
codegenExtraApiModuleDependencies apiName =
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
