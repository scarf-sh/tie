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
  "{-#"
    <+> "LANGUAGE"
    <+> "BangPatterns"
    <+> "#-}"
      <> PP.line
      <> "{-#"
    <+> "LANGUAGE"
    <+> "DuplicateRecordFields"
    <+> "#-}"
      <> PP.line
      <> "{-#"
    <+> "LANGUAGE"
    <+> "OverloadedStrings"
    <+> "#-}"
      <> PP.line
      <> "{-#"
    <+> "LANGUAGE"
    <+> "ScopedTypeVariables"
    <+> "#-}"
      <> PP.line
      <> "{-#"
    <+> "LANGUAGE"
    <+> "RankNTypes"
    <+> "#-}"
      <> PP.line
      <> "{-#"
    <+> "LANGUAGE"
    <+> "RecordWildCards"
    <+> "#-}"
      <> PP.line
      <> "module"
    <+> PP.pretty moduleName
    <+> "where"
      <> PP.line
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Control.Applicative"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Control.Exception"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Control.Monad"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Control.Monad.IO.Class"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Aeson"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Aeson.Encoding"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Aeson.Parser"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Aeson.Types"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Attoparsec.ByteString"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.List"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Map"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Maybe"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Text"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Time"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Data.Text.Encoding"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "GHC.Float"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "GHC.Int"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "GHC.Types"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Network.HTTP.Types"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Network.Wai"
      <> PP.line
      <> "import"
    <+> "qualified"
    <+> "Web.HttpApiData"

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
