{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Codegen.Schema
  ( codegenSchema,
    codegenParamSchema,
    codegenHeaderSchema,
    codegenFieldType,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.List (lookup)
import Data.OpenApi (HasPropertyName (propertyName))
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import Tie.Name
  ( inlineVariantTypeName,
    toConstructorName,
    toDataTypeName,
    toEnumConstructorName,
    toFieldName,
    toFunctionName,
    toJsonFieldName,
    toOneOfConstructorName,
    toOneOfDataTypeName,
  )
import Tie.Operation (Header (..), Param (..))
import Tie.Type
  ( BasicType (..),
    Discriminator (..),
    Enumeration (..),
    FreeFormObject (..),
    IntegerFormat (..),
    Name,
    Named (..),
    NumberFormat (..),
    ObjectType (..),
    StringFormat (..),
    Type (..),
    isArrayType,
    isBasicType,
    isEnumType,
    isObjectType,
    isOneOfType,
    namedType,
    normalizeType,
  )
import Prelude hiding (Type)

-- | Generate code for a parameter type.
codegenParamSchema :: Monad m => Param -> m (Doc ann)
codegenParamSchema Param {schema, required} =
  fmap (codegenRequiredOptionalFieldType required) $
    case schema of
      Named {} ->
        -- We are named, just defer to codegenFieldType
        pure (codegenFieldType schema)
      Unnamed typ
        | Just _enumeration <- isEnumType typ ->
            error "TODO enumeration params"
        | Just basicType <- isBasicType typ ->
            pure (codegenFieldType (Unnamed typ))
        | Just elemType <- isArrayType typ ->
            -- generate a non-empty list unconditionally
            pure (codegenArrayParameterType True elemType)
        | Just objectType <- isObjectType typ ->
            error "Invariant broken: ruled out by pathToPath"
        | otherwise ->
            undefined

-- | Generate code for a header
codegenHeaderSchema :: Header -> Doc ann
codegenHeaderSchema Header {schema, required} =
  codegenRequiredOptionalFieldType required $
    case schema of
      Just schema@Named {} ->
        -- We are named, just defer to codegenFieldType
        codegenFieldType schema
      Just (Unnamed typ)
        | Just _enumeration <- isEnumType typ ->
            error "TODO enumeration params"
        | Just basicType <- isBasicType typ ->
            codegenFieldType (Unnamed typ)
        | Just objectType <- isObjectType typ ->
            error "Invariant broken: ruled out by pathToPath"
        | otherwise ->
            undefined
      Nothing ->
        error "Header without schema"

-- | Generate code for a schema.
codegenSchema :: Monad m => Name -> Type -> m (Doc ann)
codegenSchema typName typ
  | Just Enumeration {alternatives, includeNull} <- isEnumType typ =
      pure (codegenEnumeration typName alternatives includeNull)
  | Just basicType <- isBasicType typ =
      pure (codegenBasicType typName basicType)
  | Just (discriminator, alternatives) <- isOneOfType typ = do
      let discriminatorMapping
            | Just Discriminator {propertyName, mapping} <- discriminator =
                \variantTypeName -> do
                  value <- lookup variantTypeName mapping
                  pure (propertyName, value)
            | otherwise =
                \_variantTypeName -> Nothing
      codegenOneOfType discriminatorMapping typName alternatives
  | Just objectType <- isObjectType typ =
      codegenObjectType typName objectType
  | Just elemType <- isArrayType typ =
      pure (codegenArrayType typName elemType)
  | otherwise =
      error "impossible: unknown type"

-- | Generate code for basic, primitive types
codegenBasicType :: Name -> BasicType -> Doc ann
codegenBasicType typName basicType =
  "type" <+> toDataTypeName typName <+> "=" <+> codegenFieldType (Unnamed (Basic basicType))

-- | Generate code for array types
codegenArrayType :: Name -> Named Type -> Doc ann
codegenArrayType typeName elemType =
  "type" <+> toDataTypeName typeName <+> "=" <+> "[" <+> codegenFieldType elemType <+> "]"

codegenOneOfType ::
  Monad m =>
  -- | Given a variant type name, returns the discrimintor property
  -- and value, if any
  (Name -> Maybe (Text, Text)) ->
  -- | Name of the oneOf type
  Name ->
  -- | Variants of the oneOf type
  [Named Type] ->
  m (Doc ann)
codegenOneOfType getDiscriminator typName variants = do
  let -- We derive the constructor names upfront. For unnamed types - which can still
      -- exists after normalization for e.g. basic types - we generate an inline variant
      -- type name.
      variantConstructors =
        [ (variantTypeName, name, variant)
          | (ith, variant) <- zip [1 ..] variants,
            let variantTypeName = case variant of
                  Named variantName _ ->
                    variantName
                  Unnamed typ ->
                    -- This will probably never match anything but is a
                    -- reasonable default for now
                    inlineVariantTypeName typName ith
                name = case variant of
                  Named variantName _ ->
                    toOneOfConstructorName typName variantName
                  Unnamed typ ->
                    toOneOfConstructorName typName (inlineVariantTypeName typName ith)
        ]

      decl =
        "data"
          <+> toOneOfDataTypeName typName
            <> PP.line
            <> PP.indent
              4
              ( PP.vsep
                  ( [ op
                        <+> variantName
                        <+> codegenFieldType variantType
                      | (op, (_, variantName, variantType)) <- zip ("=" : repeat "|") variantConstructors
                    ]
                      ++ [ "deriving" <+> "(" <> "Show" <> ")"
                         ]
                  )
              )

      toJson =
        "instance"
          <+> "Data.Aeson.ToJSON"
          <+> toDataTypeName typName
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( PP.vsep
                  [ "toJSON"
                      <+> "(" <> variantName
                      <+> "x" <> ")"
                      <+> "="
                      <+> "Data.Aeson.toJSON"
                      <+> "x"
                    | (_, variantName, _) <- variantConstructors
                  ]
                  <> PP.line
                  <> PP.line
                  <> PP.vsep
                    [ "toEncoding"
                        <+> "(" <> variantName
                        <+> "x" <> ")"
                        <+> "="
                        <+> "Data.Aeson.toEncoding"
                        <+> "x"
                      | (_, variantName, _) <- variantConstructors
                    ]
              )

      fromJsonVariant :: Name -> PP.Doc ann
      fromJsonVariant variantName
        | Just (property, value) <- getDiscriminator variantName =
            "(" <> "Data.Aeson.Types.withObject"
              <+> "\"" <> toDataTypeName variantName <> "\""
              <+> "$"
              <+> "\\" <> "o"
              <+> "->"
                <> PP.line
                <> PP.indent
                  4
                  ( "do"
                      <+> PP.align
                        ( "(" <> "\"" <> PP.pretty value <> "\""
                            <+> "::"
                            <+> "Data.Text.Text" <> ")"
                            <+> "<-"
                            <+> "o"
                            <+> "Data.Aeson..:"
                            <+> "\""
                              <> PP.pretty property
                              <> "\""
                              <> PP.line
                              <> "Data.Aeson.parseJSON"
                            <+> "("
                              <> "Data.Aeson.Object"
                            <+> "o"
                              <> ")"
                        )
                  )
                <> PP.line
                <> ")"
              <+> "x"
        | otherwise =
            "Data.Aeson.parseJSON" <+> "x"

      fromJson =
        "instance"
          <+> "Data.Aeson.FromJSON"
          <+> toDataTypeName typName
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( "parseJSON"
                  <+> "x"
                  <+> "="
                    <> PP.line
                    <> PP.indent
                      4
                      ( PP.concatWith
                          (\x y -> x <+> "Control.Applicative.<|>" <> PP.line <> y)
                          [ "(" <> variantConstructorName <+> "<$>" <+> fromJsonVariant variantName <> ")"
                            | (variantName, variantConstructorName, _variantType) <- variantConstructors
                          ]
                      )
              )

  pure (PP.vsep $ intersperse mempty [decl, toJson, fromJson])

codegenObjectType :: Monad m => Name -> ObjectType (Named Type) -> m (Doc ann)
codegenObjectType typName ObjectType {..}
  -- for empty, free form objects, just generate a type synonym for Value.
  | Just FreeForm <- additionalProperties,
    null properties =
      pure $
        "type" <+> toDataTypeName typName <+> "=" <+> "Data.Aeson.Value"
  -- additionalProperties: $ref some other schema
  | Just (AdditionalProperties propertyType) <- additionalProperties,
    null properties =
      let decl =
            "newtype"
              <+> toDataTypeName typName
              <+> "="
              <+> toConstructorName typName
                <> PP.line
                <> PP.indent
                  4
                  ( "(" <> "Data.Map.Map"
                      <+> "Data.Text.Text"
                      <+> "("
                        <> codegenFieldType propertyType
                        <> ")"
                        <> ")"
                        <> PP.line
                        <> "deriving"
                      <+> "(" <> "Show" <> ")"
                  )

          toJson =
            "instance"
              <+> "Data.Aeson.ToJSON"
              <+> toDataTypeName typName
              <+> "where"
                <> PP.line
                <> PP.indent
                  4
                  ( "toJSON"
                      <+> "(" <> toConstructorName typName
                      <+> "x" <> ")"
                      <+> "="
                        <> PP.line
                        <> PP.indent
                          4
                          ( "Data.Aeson.toJSON" <+> "x"
                          )
                        <> PP.line
                        <> PP.line
                        <> "toEncoding"
                      <+> "(" <> toConstructorName typName
                      <+> "x" <> ")"
                      <+> "="
                        <> PP.line
                        <> PP.indent
                          4
                          ( "Data.Aeson.toEncoding" <+> "x"
                          )
                  )

          fromJson =
            "instance"
              <+> "Data.Aeson.FromJSON"
              <+> toDataTypeName typName
              <+> "where"
                <> PP.line
                <> PP.indent
                  4
                  ( "parseJSON"
                      <+> "x"
                      <+> "="
                        <> PP.line
                        <> PP.indent
                          4
                          ( toConstructorName typName <+> "<$>" <+> "Data.Aeson.parseJSON" <+> "x"
                          )
                  )
       in pure $ PP.vsep $ intersperse mempty [decl, toJson, fromJson]
  -- additionalProperties: $ref some other schema + required properties
  | Just (AdditionalProperties propertyType) <- additionalProperties =
      error "unsupported"
  | otherwise = do
      -- Now generate for the object itself
      let orderedProperties =
            sortOn fst (HashMap.toList properties)

          dataOrNewtype = case orderedProperties of
            [_] -> "newtype"
            _ -> "data"

          decl =
            dataOrNewtype
              <+> toDataTypeName typName
              <+> "="
              <+> toConstructorName typName
                <> PP.line
                <> PP.indent
                  4
                  ( "{"
                      <> PP.line
                      <> PP.indent
                        4
                        ( PP.concatWith
                            (\x y -> x <> "," <> PP.line <> y)
                            [ toFieldName field
                                <+> "::"
                                <+> codegenRequiredOptionalFieldType
                                  (HashSet.member field requiredProperties)
                                  (codegenFieldType fieldType)
                              | (field, fieldType) <- orderedProperties
                            ]
                        )
                      <> PP.line
                      <> "}"
                      <> PP.line
                      <> "deriving"
                      <+> "("
                        <> "Show"
                        <> ")"
                  )

          toJson =
            "instance"
              <+> "Data.Aeson.ToJSON"
              <+> toDataTypeName typName
              <+> "where"
                <> PP.line
                <> PP.indent
                  4
                  ( "toJSON"
                      <+> toConstructorName typName
                      <+> "{..}"
                      <+> "="
                      <+> "Data.Aeson.object"
                        <> PP.line
                        <> PP.indent
                          4
                          ( "(" <> "["
                              <+> PP.align
                                ( PP.concatWith
                                    (\x y -> x <> "," <> PP.line <> y)
                                    [ "\"" <> toJsonFieldName field <> "\"" <+> "Data.Aeson..=" <+> toFieldName field
                                      | (field, _) <- orderedProperties,
                                        HashSet.member field requiredProperties
                                    ]
                                )
                                <> PP.line
                                <> "]"
                                <> PP.line
                                <> PP.concatWith
                                  (\x y -> x <> PP.line <> y)
                                  [ ( "++"
                                        <+> "["
                                        <+> "\"" <> toJsonFieldName field <> "\""
                                        <+> "Data.Aeson..="
                                        <+> toFieldName field
                                        <+> "|"
                                        <+> "Just"
                                        <+> toFieldName field
                                        <+> "<-"
                                        <+> "[" <> toFieldName field <> "]"
                                        <+> "]"
                                    )
                                    | (field, _) <- orderedProperties,
                                      not (HashSet.member field requiredProperties)
                                  ]
                                <> ")"
                          )
                        <> PP.line
                        <> PP.line
                        <> "toEncoding"
                      <+> toConstructorName typName
                      <+> "{..}"
                      <+> "="
                      <+> "Data.Aeson.Encoding.pairs"
                        <> PP.line
                        <> PP.indent
                          4
                          ( "("
                              <+> PP.align
                                ( PP.concatWith
                                    (\x y -> x <+> "<>" <> PP.line <> y)
                                    [ if HashSet.member field requiredProperties
                                        then
                                          "Data.Aeson.Encoding.pair"
                                            <+> "\"" <> toJsonFieldName field <> "\""
                                            <+> "(" <> "Data.Aeson.toEncoding"
                                            <+> toFieldName field <> ")"
                                        else
                                          "maybe"
                                            <+> "mempty"
                                            <+> "("
                                              <> "Data.Aeson.Encoding.pair"
                                            <+> "\"" <> toJsonFieldName field <> "\""
                                            <+> "."
                                            <+> "Data.Aeson.toEncoding" <> ")"
                                            <+> toFieldName field
                                      | (field, _) <- orderedProperties
                                    ]
                                )
                                <> PP.line
                                <> ")"
                          )
                  )

          fromOptOrReq field
            | HashSet.member field requiredProperties = "Data.Aeson..:"
            | otherwise = "Data.Aeson..:?"

          fromJson =
            "instance"
              <+> "Data.Aeson.FromJSON"
              <+> toDataTypeName typName
              <+> "where"
                <> PP.line
                <> PP.indent
                  4
                  ( "parseJSON"
                      <+> "="
                      <+> "Data.Aeson.withObject"
                      <+> "\"" <> toDataTypeName typName <> "\""
                      <+> "$"
                      <+> "\\" <> "o"
                      <+> "->"
                        <> PP.line
                        <> PP.indent
                          4
                          ( toConstructorName typName
                              <> PP.line
                              <> PP.indent
                                4
                                ( PP.vsep
                                    [ op <+> "o" <+> fromOptOrReq fieldName <+> "\"" <> toJsonFieldName fieldName <> "\""
                                      | (op, (fieldName, _)) <- zip ("<$>" : repeat "<*>") orderedProperties
                                    ]
                                )
                          )
                  )
       in pure (PP.vsep $ intersperse mempty [decl, toJson, fromJson])

codegenRequiredOptionalFieldType :: Bool -> Doc ann -> Doc ann
codegenRequiredOptionalFieldType True doc = doc
codegenRequiredOptionalFieldType False doc = "(" <> "Data.Maybe.Maybe" <+> "(" <> doc <> ")" <> ")"

-- | Special casing for generating parameter types for arrays
codegenArrayParameterType ::
  -- | Non-Empty?
  Bool ->
  -- | Element type
  Named Type ->
  Doc ann
codegenArrayParameterType requireNonEmpty elemType
  | requireNonEmpty =
      "Data.List.NonEmpty.NonEmpty" <+> "(" <+> codegenFieldType elemType <+> ")"
  | otherwise =
      "[" <+> codegenFieldType elemType <+> "]"

codegenFieldType :: Named Type -> Doc ann
codegenFieldType namedType = case namedType of
  Named name _ -> toDataTypeName name
  Unnamed typ -> case typ of
    AllOf {} -> "error: allOf"
    AnyOf {} -> "error: anyOf"
    OneOf {} -> "error: oneOf"
    Not {} -> "error: not"
    Basic basicType -> case basicType of
      TyString format -> case format of
        Nothing ->
          "Data.Text.Text"
        Just FormatUnknown {} ->
          "Data.Text.Text"
        Just FormatDate ->
          "Data.Time.Day"
        Just FormatDateTime ->
          "Data.Time.UTCTime"
        Just _otherFormat ->
          -- TODO consider other formats
          "Data.Text.Text"
      TyEnum {} -> "error: Enum"
      TyNumber format -> case format of
        Nothing ->
          "GHC.Float.Double"
        Just FormatDouble ->
          "GHC.Float.Double"
        Just FormatFloat ->
          "GHC.Float.Float"
        Just (NumberFormatUnknown _) ->
          -- Default to Double in case of unknown
          -- TODO warn about unknown formats
          "GHC.Float.Double"
      TyInteger format -> case format of
        Nothing ->
          "GHC.Int.Int"
        Just FormatInt32 ->
          "GHC.Int.Int32"
        Just FormatInt64 ->
          "GHC.Int.Int64"
        Just (IntegerFormatUnknown _) ->
          -- Default to Int in case of unknown
          -- TODO warn about unknown formats
          "GHC.Int.Int"
      TyBoolean ->
        "GHC.Types.Bool"
      TyHaskellType _ escapedHaskellType ->
        PP.pretty escapedHaskellType
    Object objectType -> "Data.Aeson.Value"
    Array elemType ->
      codegenArrayParameterType False elemType

-- | Generate the Haskell code for enumeration types
codegenEnumeration :: Name -> [Text] -> Bool -> Doc ann
codegenEnumeration typName alternatives _includeNull =
  let dataDecl =
        "data"
          <+> toDataTypeName typName
            <> PP.line
            <> PP.indent
              4
              ( "="
                  <+> PP.concatWith
                    (\x y -> x <> PP.line <> "|" <+> y)
                    (map (toEnumConstructorName typName) alternatives)
                    <> PP.line
                    <> "deriving"
                  <+> "(" <> "Eq" <> ","
                  <+> "Show" <> ")"
              )
      toJSON =
        "instance"
          <+> "Data.Aeson.ToJSON"
          <+> toDataTypeName typName
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( "toJSON"
                  <+> "x"
                  <+> "="
                  <+> "case"
                  <+> "x"
                  <+> "of"
                    <> PP.line
                    <> PP.indent
                      4
                      ( PP.vsep
                          [ toEnumConstructorName typName alt <+> "->" <+> "\"" <> PP.pretty alt <> "\""
                            | alt <- alternatives
                          ]
                      )
                    <> PP.line
                    <> PP.line
                    <> "toEncoding"
                  <+> "x"
                  <+> "="
                  <+> "case"
                  <+> "x"
                  <+> "of"
                    <> PP.line
                    <> PP.indent
                      4
                      ( PP.vsep
                          [ toEnumConstructorName typName alt <+> "->" <+> "Data.Aeson.Encoding.text" <+> "\"" <> PP.pretty alt <> "\""
                            | alt <- alternatives
                          ]
                      )
              )
      fromJSON =
        "instance"
          <+> "Data.Aeson.FromJSON"
          <+> toDataTypeName typName
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( "parseJSON"
                  <+> "="
                  <+> "Data.Aeson.withText"
                  <+> "\"" <> toDataTypeName typName <> "\""
                  <+> "$"
                  <+> "\\" <> "s"
                  <+> "->"
                    <> PP.line
                    <> PP.indent
                      4
                      ( "case"
                          <+> "s"
                          <+> "of"
                            <> PP.line
                            <> PP.indent
                              4
                              ( PP.vsep
                                  ( [ "\"" <> PP.pretty alt <> "\"" <+> "->" <+> "pure" <+> toEnumConstructorName typName alt
                                      | alt <- alternatives
                                    ]
                                      ++ ["_" <+> "->" <+> "fail" <+> "\"invalid enum value\""]
                                  )
                              )
                      )
              )
      toHttpApiData =
        "instance"
          <+> "Web.HttpApiData.ToHttpApiData"
          <+> toDataTypeName typName
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( "toQueryParam"
                  <+> "x"
                  <+> "="
                  <+> "case"
                  <+> "x"
                  <+> "of"
                    <> PP.line
                    <> PP.indent
                      4
                      ( PP.vsep
                          [ toEnumConstructorName typName alt <+> "->" <+> "\"" <> PP.pretty alt <> "\""
                            | alt <- alternatives
                          ]
                      )
              )
      fromHttpApiData =
        "instance"
          <+> "Web.HttpApiData.FromHttpApiData"
          <+> toDataTypeName typName
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( "parseUrlPiece"
                  <+> "x"
                  <+> "="
                    <> PP.line
                    <> PP.indent
                      4
                      ( "case"
                          <+> "x"
                          <+> "of"
                            <> PP.line
                            <> PP.indent
                              4
                              ( PP.vsep
                                  ( [ "\"" <> PP.pretty alt <> "\"" <+> "->" <+> "pure" <+> toEnumConstructorName typName alt
                                      | alt <- alternatives
                                    ]
                                      ++ ["_" <+> "->" <+> "Left" <+> "\"invalid enum value\""]
                                  )
                              )
                      )
              )
   in PP.vsep
        ( intersperse
            mempty
            [dataDecl, toJSON, fromJSON, toHttpApiData, fromHttpApiData]
        )
