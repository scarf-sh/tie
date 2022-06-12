{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Codegen.Operation
  ( codegenOperation,
    codegenOperations,
  )
where

import qualified Data.Map.Strict as Map
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import Tie.Codegen.Response (codegenResponses)
import Tie.Codegen.Schema (codegenFieldType, codegenParamSchema)
import Tie.Name
  ( Name,
    toApiMemberName,
    toApiResponseTypeName,
    toParamBinder,
    toParamName,
  )
import Tie.Operation
  ( Operation (..),
    Param (..),
    Path,
    PathSegment (..),
    RequestBody (..),
    Response (..),
  )
import Tie.Resolve (Resolver)

codegenOperations :: Monad m => Resolver m -> [Operation] -> m (PP.Doc ann)
codegenOperations resolver operations = do
  let groupedOperations :: Map.Map Path [Operation]
      groupedOperations =
        Map.fromListWith
          (<>)
          [ (path, [operation])
            | operation@Operation {path} <- operations
          ]

  dataApiDecl <- codegenApiType resolver operations
  operationsCode <- traverse (codegenOperation resolver) (Map.elems groupedOperations)
  let apiDecl =
        -- TODO instead of "application" take name from openapi spec
        "application" <+> "::" <+> "(" <> "Control.Monad.IO.Class.MonadIO" <+> "m" <> ")" <+> "=>" <+> "(" <> "forall" <+> "a" <+> "." <+> "Network.Wai.Request" <+> "->" <+> "m"
          <+> "a"
          <+> "->"
          <+> "IO"
          <+> "a" <> ")"
          <+> "->"
          <+> "Api"
          <+> "m"
          <+> "->"
          <+> "Network.Wai.Application"
          <+> "->"
          <+> "Network.Wai.Application" <> PP.line
            <> "application"
          <+> "run"
          <+> "api"
          <+> "notFound"
          <+> "request"
          <+> "respond"
          <+> "=" <> PP.line
            <> PP.indent
              4
              ( "case" <+> "Network.Wai.pathInfo" <+> "request" <+> "of" <> PP.line
                  <> PP.indent
                    4
                    ( PP.concatWith
                        (\x y -> x <> PP.line <> PP.line <> y)
                        ( operationsCode
                            ++ [ "_" <+> "->" <> PP.line
                                   <> PP.indent 4 ("notFound" <+> "request" <+> "respond")
                               ]
                        )
                    )
                  <> PP.line
                  <> "where"
                  <> PP.line
                  <> PP.indent
                    4
                    ( "unsupportedMethod" <+> "_" <+> "=" <> PP.line
                        <> PP.indent
                          4
                          ( "respond" <+> "(" <> "Network.Wai.responseBuilder"
                              <+> "(" <> "toEnum"
                              <+> "405" <> ")"
                              <+> "[]"
                              <+> "mempty" <> ")"
                          )
                    )
              )

      inlineablePragma = 
        "{-#" <+> "INLINABLE" <+> "application" <+> "#-}"

  pure (dataApiDecl <> PP.line <> PP.line <> apiDecl <> PP.line <> inlineablePragma)

codegenApiType :: Monad m => Resolver m -> [Operation] -> m (PP.Doc ann)
codegenApiType resolver operations = do
  operationsFieldsCode <- traverse (codegenApiTypeOperation resolver) operations
  let fieldsCode =
        PP.concatWith (\x y -> x <> "," <> PP.line <> y) operationsFieldsCode

      dataDecl =
        "data" <+> "Api" <+> "m" <+> "=" <+> "Api" <+> "{" <> PP.line
          <> PP.indent 4 fieldsCode
          <> PP.line
          <> "}"
  pure dataDecl

codegenApiTypeOperation :: Monad m => Resolver m -> Operation -> m (PP.Doc ann)
codegenApiTypeOperation resolver Operation {..} = do
  paramsCode <-
    sequence $
      [ codegenParamSchemaAndComment param
        | VariableSegment param@Param {summary} <- path
      ]
        ++ [ codegenParamSchemaAndComment param
             | param@Param {summary} <- queryParams
           ]
        ++ [ codegenParamSchemaAndComment param
             | param@Param {summary} <- headerParams
           ]
  pure $
    codegenApiMemberComment summary
      <> toApiMemberName name <+> "::"
      <> PP.line
      <> PP.indent
        4
        ( PP.concatWith
            (\x y -> x <+> "->" <> PP.line <> y)
            ( paramsCode
                ++ [ codegenRequestBodyComment body
                       <> codegenFieldType jsonRequestBodyContent
                     | Just body@RequestBody {jsonRequestBodyContent} <- [requestBody]
                   ]
                ++ ["m" <+> toApiResponseTypeName name]
            )
        )
  where
    codegenApiMemberComment mcomment = case mcomment of
      Nothing -> mempty
      Just comment -> "-- |" <+> PP.pretty comment <> PP.line

    codegenParamComment Param {name, summary} = case summary of
      Nothing ->
        "--" <+> "@" <> toParamName name <> "@" <> PP.line
      Just comment ->
        "--" <+> "@" <> toParamName name <> "@" <+> PP.pretty comment <> PP.line

    codegenRequestBodyComment RequestBody {description} = case description of
      Nothing ->
        mempty
      Just comment ->
        "--" <+> PP.pretty comment <> PP.line

    codegenParamSchemaAndComment param = do
      code <- codegenParamSchema param
      pure (codegenParamComment param <> code)

codegenOperation :: Monad m => Resolver m -> [Operation] -> m (PP.Doc ann)
codegenOperation resolver operations@(Operation {path} : _) =
  pure $
    codegenPathGuard path $
      codegenMethodGuard
        [ ( method,
            codegenQueryParamsGuard queryParams $
              codegenHeaderParamsGuard headerParams $
                codegenRequestBodyGuard requestBody $
                  ( codegenCallApiMember name path queryParams headerParams requestBody
                  )
          )
          | operation@Operation
              { name,
                path,
                queryParams,
                headerParams,
                method,
                requestBody
              } <-
              operations
        ]

codegenCallApiMember :: Name -> Path -> [Param] -> [Param] -> Maybe RequestBody -> PP.Doc ann
codegenCallApiMember operationName path queryParams headerParams requestBody =
  "run" <+> "request" <+> "(" <> "do" <> PP.line
    <> PP.indent
      4
      ( "response" <+> "<-"
          <+> PP.hsep
            ( concat
                [ [toApiMemberName operationName, "api"],
                  [toParamBinder name | VariableSegment Param {name} <- path],
                  [toParamBinder name | Param {name} <- queryParams],
                  [toParamBinder name | Param {name} <- headerParams],
                  ["body" | Just {} <- [requestBody]]
                ]
            )
            <> PP.line
            <> "Control.Monad.IO.Class.liftIO"
          <+> "(" <> "respond"
          <+> "$!"
          <+> "(" <> "toResponse"
          <+> "response" <> ")" <> ")"
      )
    <> PP.line
    <> ")"

codegenPathGuard :: Path -> PP.Doc ann -> PP.Doc ann
codegenPathGuard path continuation =
  codegenPathPattern path <+> "->" <> PP.line
    <> PP.indent
      4
      ( codegenParamsGuard
          codegenPathParamGuard
          [param | VariableSegment param <- path]
          continuation
      )

codegenPathPattern :: Path -> PP.Doc ann
codegenPathPattern path =
  "["
    <> PP.concatWith
      (\x y -> x <> "," <+> y)
      (map codegenPathSegmentPattern path)
    <> "]"

codegenPathSegmentPattern :: PathSegment Param -> PP.Doc ann
codegenPathSegmentPattern segment = case segment of
  StaticSegment literal ->
    "\"" <> PP.pretty literal <> "\""
  VariableSegment Param {name} ->
    toParamBinder name

codegenMethodGuard :: [(Text, PP.Doc ann)] -> PP.Doc ann
codegenMethodGuard methodBodies =
  "case" <+> "Network.Wai.requestMethod" <+> "request" <+> "of" <> PP.line
    <> PP.indent
      4
      ( PP.vsep $
          [ "\"" <> PP.pretty method <> "\"" <+> "->" <> PP.line <> PP.indent 4 body
            | (method, body) <- methodBodies
          ]
            ++ [ "x" <+> "->" <> PP.line <> PP.indent 4 ("unsupportedMethod" <+> "x")
               ]
      )

codegenRequestBodyGuard :: Maybe RequestBody -> PP.Doc ann -> PP.Doc ann
codegenRequestBodyGuard requestBody continuation = case requestBody of
  Nothing ->
    continuation
  Just RequestBody {jsonRequestBodyContent} ->
    let parsers =
          -- TODO support forms
          ["jsonBodyParser"]

        parsersList =
          "[" <> PP.concatWith (\x y -> x <> "," <+> y) parsers <> "]"
     in "parseRequestBody" <+> parsersList <+> "(" <> "\\" <> "body" <+> "request" <+> "respond" <+> "->" <> PP.line
          <> PP.indent 4 continuation
          <> ")" <+> "request" <+> "respond"

codegenQueryParamsGuard :: [Param] -> PP.Doc ann -> PP.Doc ann
codegenQueryParamsGuard =
  codegenParamsGuard codegenQueryParamGuard

codegenHeaderParamsGuard :: [Param] -> PP.Doc ann -> PP.Doc ann
codegenHeaderParamsGuard =
  codegenParamsGuard codegenHeaderGuard

codegenParamsGuard :: (Param -> PP.Doc ann -> PP.Doc ann) -> [Param] -> PP.Doc ann -> PP.Doc ann
codegenParamsGuard codegenParam params continuation =
  foldr
    ($)
    continuation
    [codegenParam param | param <- params]

codegenPathParamGuard :: Param -> PP.Doc ann -> PP.Doc ann
codegenPathParamGuard Param {name} continuation =
  "pathVariable" <+> toParamBinder name <+> "(" <> "\\" <> toParamBinder name <+> "request" <+> "respond" <+> "->" <> PP.line
    <> PP.indent 4 continuation
    <> ")" <+> "request" <+> "respond"

codegenQueryParamGuard :: Param -> PP.Doc ann -> PP.Doc ann
codegenQueryParamGuard Param {name, required} continuation
  | required =
    "requiredQueryParameter" <+> "\"" <> toParamName name <> "\"" <+> "(" <> "\\" <> toParamBinder name <+> "request" <+> "respond" <+> "->" <> PP.line
      <> PP.indent 4 continuation
      <> ")" <+> "request" <+> "respond"
  | otherwise =
    "optionalQueryParameter" <+> "\"" <> toParamName name <> "\"" <+> "False" <+> "(" <> "\\" <> toParamBinder name <+> "request" <+> "respond" <+> "->" <> PP.line
      <> PP.indent 4 continuation
      <> ")" <+> "request" <+> "respond"

codegenHeaderGuard :: Param -> PP.Doc ann -> PP.Doc ann
codegenHeaderGuard Param {name, required} continuation
  | required =
    "requiredHeader" <+> "\"" <> toParamName name <> "\"" <+> "(" <> "\\" <> toParamBinder name <+> "request" <+> "respond" <+> "->" <> PP.line
      <> PP.indent 4 continuation
      <> ")" <+> "request" <+> "respond"
  | otherwise =
    "optionalHeader" <+> "\"" <> toParamName name <> "\"" <+> "(" <> "\\" <> toParamBinder name <+> "request" <+> "respond" <+> "->" <> PP.line
      <> PP.indent 4 continuation
      <> ")" <+> "request" <+> "respond"
