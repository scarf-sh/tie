{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tie.Name
  ( PackageName,
    ApiName,
    Name,
    fromText,
    cabalFileName,
    toDataTypeName,
    toOneOfDataTypeName,
    toOneOfConstructorName,
    toFunctionName,
    toConstructorName,
    toFieldName,
    toJsonFieldName,
    toParamName,
    toParamBinder,
    toApiTypeName,
    toSchemaHaskellFileName,
    toSchemaHaskellModuleName,
    toOperationHaskellFileName,
    toOperationHaskellModuleName,
    toResponseHaskellFileName,
    toResponseHaskellModuleName,
    toApiResponseTypeName,
    toApiResponseConstructorName,
    toApiDefaultResponseConstructorName,
    toApiMemberName,
    toEnumConstructorName,
    apiHaskellModuleName,
    apiHaskellFileName,
    requestHaskellModuleName,
    requestHaskellFileName,
    responseHaskellModuleName,
    responseHaskellFileName,
    inlineObjectTypeName,
    additionalPropertiesTypeName,
    inlineVariantTypeName,
    inlineArrayElementTypeName,
    operationParamTypeName,
    operationRequestBodyName,
    apiResponseConstructorName,
    apiDefaultResponseConstructorName,
    extractHaskellModule,
  )
where

import Data.Char (isUpper, toLower, toUpper)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Prettyprinter as PP

-- | Name of the API to generate code for
type ApiName = Text

-- | Cabal package name
type PackageName = Text

-- | Names identify things in the OpenApi universe. Name's are coming directly
-- from the OpenApi spec.
newtype Name = Name {unName :: Text}
  deriving (IsString, Eq, Ord, Show, Hashable)

fromText :: Text -> Name
fromText = Name

cabalFileName :: PackageName -> FilePath
cabalFileName packageName =
  Text.unpack packageName <> ".cabal"

apiHaskellModuleName :: ApiName -> Text
apiHaskellModuleName apiName =
  apiName <> ".Api"

apiHaskellFileName :: ApiName -> FilePath
apiHaskellFileName apiName =
  haskellModuleToFilePath apiName <> "/Api.hs"

requestHaskellModuleName :: ApiName -> Text
requestHaskellModuleName apiName =
  apiName <> ".Request"

requestHaskellFileName :: ApiName -> FilePath
requestHaskellFileName apiName =
  haskellModuleToFilePath apiName <> "/Request.hs"

responseHaskellModuleName :: ApiName -> Text
responseHaskellModuleName apiName =
  apiName <> ".Response"

responseHaskellFileName :: ApiName -> FilePath
responseHaskellFileName apiName =
  haskellModuleToFilePath apiName <> "/Response.hs"

toSchemaHaskellModuleName :: ApiName -> Name -> Text
toSchemaHaskellModuleName apiName (Name name) =
  Text.pack $ Text.unpack apiName <> ".Schemas." <> capitalizeFirstLetter (Text.unpack name)

toSchemaHaskellFileName :: ApiName -> Name -> FilePath
toSchemaHaskellFileName apiName (Name name) =
  haskellModuleToFilePath apiName <> "/Schemas/" <> capitalizeFirstLetter (Text.unpack name) <> ".hs"

haskellModuleToFilePath :: ApiName -> FilePath
haskellModuleToFilePath =
  Text.unpack . Text.replace "." "/"

toOperationHaskellModuleName :: ApiName -> Name -> Text
toOperationHaskellModuleName apiName (Name name) =
  Text.pack $ Text.unpack apiName <> ".Api." <> capitalizeFirstLetter (Text.unpack name)

toOperationHaskellFileName :: ApiName -> Name -> FilePath
toOperationHaskellFileName apiName (Name name) =
  haskellModuleToFilePath apiName <> "/Api/" <> capitalizeFirstLetter (Text.unpack name) <> ".hs"

toResponseHaskellModuleName :: ApiName -> Name -> Text
toResponseHaskellModuleName apiName (Name name) =
  Text.pack $ Text.unpack apiName <> ".Response." <> capitalizeFirstLetter (Text.unpack name)

toResponseHaskellFileName :: ApiName -> Name -> FilePath
toResponseHaskellFileName apiName (Name name) =
  haskellModuleToFilePath apiName <> "/Response/" <> capitalizeFirstLetter (Text.unpack name) <> ".hs"

toApiTypeName :: Name -> PP.Doc ann
toApiTypeName =
  toDataTypeName

toJsonFieldName :: Name -> PP.Doc ann
toJsonFieldName = PP.pretty . unName

toDataTypeName :: Name -> PP.Doc ann
toDataTypeName =
  PP.pretty . Text.pack . capitalizeFirstLetter . toCamelCase . Text.unpack . unName

toOneOfDataTypeName :: Name -> PP.Doc ann
toOneOfDataTypeName =
  PP.pretty . Text.pack . capitalizeFirstLetter . toCamelCase . Text.unpack . unName

toOneOfConstructorName :: Name -> Name -> PP.Doc ann
toOneOfConstructorName (Name oneOfType) (Name variant) =
  PP.pretty $
    Text.pack $
      escapeKeyword $
        capitalizeFirstLetter (toCamelCase $ Text.unpack oneOfType)
          <> capitalizeFirstLetter (toCamelCase $ Text.unpack variant)

toConstructorName :: Name -> PP.Doc ann
toConstructorName = toDataTypeName

toFunctionName :: Name -> PP.Doc ann
toFunctionName =
  PP.pretty . Text.pack . escapeKeyword . lowerFirstLetter . toCamelCase . Text.unpack . unName

toFieldName :: Name -> PP.Doc ann
toFieldName =
  PP.pretty . Text.pack . escapeKeyword . lowerFirstLetter . toCamelCase . Text.unpack . unName

-- | Returns the name as written, should be used within quotes only.
toParamName :: Name -> PP.Doc ann
toParamName =
  PP.pretty . filterNUL . unName
  where
    -- Filter away '\0' to support the weird cookie trick
    -- (see test/golden/weird-cookie-trick.yaml)
    filterNUL = Text.filter (/= '\0')

toParamBinder :: Name -> PP.Doc ann
toParamBinder =
  PP.pretty . Text.pack . escapeKeyword . lowerFirstLetter . ("__" <>) . Text.unpack . unName

operationParamTypeName :: Name -> Name -> Name
operationParamTypeName (Name operationName) (Name paramName) =
  Name $
    Text.pack $
      escapeKeyword $
        capitalizeFirstLetter (Text.unpack operationName)
          <> capitalizeFirstLetter (Text.unpack paramName)
          <> "Param"

operationRequestBodyName :: Name -> Name
operationRequestBodyName (Name operationName) =
  Name $
    Text.pack $
      escapeKeyword $
        capitalizeFirstLetter (Text.unpack operationName)
          <> "RequestBody"

toApiMemberName :: Name -> PP.Doc ann
toApiMemberName =
  PP.pretty . Text.pack . escapeKeyword . lowerFirstLetter . toCamelCase . Text.unpack . unName

toApiResponseTypeName :: Name -> PP.Doc ann
toApiResponseTypeName =
  PP.pretty . Text.pack . escapeKeyword . (<> "Response") . capitalizeFirstLetter . Text.unpack . unName

toApiResponseConstructorName :: Name -> Int -> PP.Doc ann
toApiResponseConstructorName name statusCode =
  PP.pretty . Text.pack . escapeKeyword . (<> show statusCode) . (<> "Response") . capitalizeFirstLetter . Text.unpack . unName $ name

apiResponseConstructorName :: Name -> Int -> Name
apiResponseConstructorName name statusCode =
  Name . Text.pack . escapeKeyword . (<> show statusCode) . (<> "ResponseBody") . capitalizeFirstLetter . Text.unpack . unName $ name

toApiDefaultResponseConstructorName :: Name -> PP.Doc ann
toApiDefaultResponseConstructorName name =
  PP.pretty . Text.pack . escapeKeyword . (<> "DefaultResponse") . capitalizeFirstLetter . Text.unpack . unName $ name

apiDefaultResponseConstructorName :: Name -> Name
apiDefaultResponseConstructorName name =
  Name . Text.pack . escapeKeyword . (<> "DefaultResponseBody") . capitalizeFirstLetter . Text.unpack . unName $ name

toEnumConstructorName :: Name -> Text -> PP.Doc ann
toEnumConstructorName (Name typName) variant =
  PP.pretty $
    Text.pack $
      escapeKeyword $
        toCamelCase $
          capitalizeFirstLetter (Text.unpack typName)
            <> capitalizeFirstLetter (Text.unpack variant)

-- | Constructs a name for an object defined inline. Based on the containing data
-- type as well as the field name.
inlineObjectTypeName :: Name -> Name -> Name
inlineObjectTypeName (Name parentType) (Name fieldName) =
  Name $
    Text.pack $
      escapeKeyword $
        capitalizeFirstLetter (Text.unpack parentType)
          <> capitalizeFirstLetter (Text.unpack fieldName)

-- | Generate a name for additionalProperties type name within an
-- ObjectType.
additionalPropertiesTypeName :: Name -> Name
additionalPropertiesTypeName (Name parentObjectType) =
  Name $
    Text.pack $
      escapeKeyword $
        capitalizeFirstLetter (Text.unpack parentObjectType)
          <> "AdditionalProperties"

-- | Construct a name for an inline type in a oneOf.
inlineVariantTypeName :: Name -> Int -> Name
inlineVariantTypeName (Name parentType) ith =
  Name $
    Text.pack $
      escapeKeyword $
        capitalizeFirstLetter (Text.unpack parentType) <> "OneOf" <> show ith

inlineArrayElementTypeName :: Name -> Name
inlineArrayElementTypeName (Name parentType) =
  Name $
    Text.pack $
      escapeKeyword $
        capitalizeFirstLetter (Text.unpack parentType) <> "Elem"

lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x : xs) = toLower x : xs

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter [] = []
capitalizeFirstLetter (x : xs) = toUpper x : xs

escapeKeyword :: String -> String
escapeKeyword input = haskelify $ case input of
  "type" -> "type'"
  "class" -> "class'"
  "where" -> "where'"
  "case" -> "case'"
  "of" -> "of'"
  "data" -> "data'"
  "import" -> "import'"
  "qualified" -> "qualified'"
  "as" -> "as'"
  "instance" -> "instance'"
  "module" -> "module'"
  "pattern" -> "pattern'"
  _ -> input

haskelify :: String -> String
haskelify = concatMap escape
  where
    escape c = case c of
      '-' -> "_"
      '\0' -> "NUL"
      _ -> [c]

toCamelCase :: String -> String
toCamelCase input =
  (prefix <>)
    . (<> suffix)
    . concat
    . map (capitalizeFirstLetter . Text.unpack)
    . Text.split (\c -> c == '_' || c == '-')
    . Text.pack
    $ input
  where
    -- Preserve leading and trailing _
    prefix = takeWhile ('_' ==) input
    suffix = takeWhile ('_' ==) (reverse input)

-- @
--  extractHaskellModules "Int" = []
--  extractHaskellModules "GHC.Types.Int" == ["GHC.Types"]
--  extractHaskellModules "Scarf.Hashids.Hashid GHC.Types.Int == ["Scarf.Hashids", "GHC.Types"]"
-- @
extractHaskellModule :: Text -> [Text]
extractHaskellModule =
  let extractModule ty =
        case List.init (Text.splitOn "." ty) of
          [] ->
            []
          xs ->
            [Text.intercalate "." xs]
   in concatMap extractModule . Text.words
