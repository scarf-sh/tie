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
        "application" <+> "::" <+> "(" <> "Control.Monad.Catch.MonadCatch" <+> "m" <> "," <+> "Control.Monad.IO.Class.MonadIO" <+> "m" <> ")" <+> "=>" <+> "(" <> "forall" <+> "a" <+> "." <+> "Network.Wai.Request" <+> "->" <+> "m"
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
                        <> PP.line
                        <> "invalidRequest" <+> "_" <+> "="
                        <> PP.line
                        <> PP.indent
                          4
                          ( "respond" <+> "(" <> "Network.Wai.responseBuilder"
                              <+> "(" <> "toEnum"
                              <+> "400" <> ")"
                              <+> "[]"
                              <+> "mempty" <> ")"
                          )
                    )
              )

  pure (dataApiDecl <> PP.line <> PP.line <> apiDecl)

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
        "--" <+> "@" <> toParamBinder name <> "@" <> PP.line
      Just comment ->
        "--" <+> "@" <> toParamBinder name <> "@" <+> PP.pretty comment <> PP.line

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
              codegenRequestBodyGuard requestBody $
                ( codegenCallApiMember name path queryParams requestBody
                )
          )
          | operation@Operation
              { name,
                path,
                queryParams,
                method,
                requestBody
              } <-
              operations
        ]

codegenCallApiMember :: Name -> Path -> [Param] -> Maybe RequestBody -> PP.Doc ann
codegenCallApiMember operationName path queryParams requestBody =
  "run" <+> "request" <+> "(" <> "do" <> PP.line
    <> PP.indent
      4
      ( "response" <+> "<-" <+> "Control.Monad.Catch.handle" <+> "pure" <+> "(" <> toApiMemberName operationName
          <+> "api"
          <+> PP.hsep
            ( [toParamBinder name | VariableSegment Param {name} <- path]
                ++ [toParamBinder name | Param {name} <- queryParams]
            )
          <+> (maybe mempty (\_ -> "body") requestBody)
            <> ")"
            <> PP.line
            <> "Control.Monad.IO.Class.liftIO"
          <+> "(" <> "respond"
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
      ( foldr
          ($)
          continuation
          [codegenPathParamGuard param | VariableSegment param <- path]
      )

codegenPathParamGuard :: Param -> PP.Doc ann -> PP.Doc ann
codegenPathParamGuard Param {name} continuation =
  "case" <+> "Web.HttpApiData.parseUrlPiece" <+> toParamBinder name <+> "of" <> PP.line
    <> PP.indent
      4
      ( "Left" <+> "_" <+> "->" <+> "invalidRequest" <+> "\"" <> toParamName name <> "\"" <> PP.line
          <> "Right" <+> toParamBinder name <+> "->"
          <> PP.line
          <> PP.indent 4 continuation
      )

codegenPathPattern :: Path -> PP.Doc ann
codegenPathPattern path =
  "["
    <+> PP.concatWith
      (\x y -> x <> "," <+> y)
      (map codegenPathSegmentPattern path)
    <+> "]"

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
  Nothing -> continuation
  Just _body ->
    "do"
      <+> PP.align
        ( "result" <+> "<-" <+> "Data.Attoparsec.ByteString.parseWith" <+> "(" <> "Network.Wai.getRequestBodyChunk" <+> "request" <> ")" <+> "Data.Aeson.Parser.json'" <+> "mempty" <> PP.line
            <> "case" <+> "Data.Attoparsec.ByteString.eitherResult" <+> "result" <+> "of"
            <> PP.line
            <> PP.indent
              4
              ( "Left" <+> "err" <+> "->" <+> "invalidRequest" <+> "err" <> PP.line
                  <> "Right" <+> "bodyValue" <+> "->"
                  <> PP.line
                  <> PP.indent
                    4
                    ( "case" <+> "Data.Aeson.Types.parseEither" <+> "Data.Aeson.parseJSON" <+> "bodyValue" <+> "of" <> PP.line
                        <> PP.indent
                          4
                          ( "Left" <+> "err" <+> "->" <+> "invalidRequest" <+> "err" <> PP.line
                              <> "Right" <+> "body" <+> "->"
                              <> PP.line
                              <> PP.indent 4 continuation
                          )
                    )
              )
        )

codegenQueryParamsGuard :: [Param] -> PP.Doc ann -> PP.Doc ann
codegenQueryParamsGuard params continuation =
  foldr
    ($)
    continuation
    [codegenQueryParamGuard param | param <- params]

codegenQueryParamGuard :: Param -> PP.Doc ann -> PP.Doc ann
codegenQueryParamGuard Param {name, required} continuation
  | required =
    "case" <+> "Control.Monad.join" <+> "(" <> "fmap" <+> "(" <> "fmap" <+> "(" <> "Web.HttpApiData.parseUrlPiece" <+> "." <+> "Data.Text.Encoding.decodeUtf8" <> ")" <> ")" <+> "("
      <> "Data.List.lookup" <+> "\""
      <> toParamName name
      <> "\"" <+> "("
      <> "Network.Wai.queryString" <+> "request"
      <> ")"
      <> ")"
      <> ")" <+> "of"
      <> PP.line
      <> PP.indent
        4
        ( "Nothing" <+> "->" <> PP.line
            <> PP.indent
              4
              ( "invalidRequest" <+> "\"request body\""
              )
            <> PP.line
            <> "Just" <+> "("
            <> "Left" <+> "err"
            <> ")" <+> "->"
            <> PP.line
            <> PP.indent
              4
              ( "invalidRequest" <+> "\"request body\""
              )
            <> PP.line
            <> "Just" <+> "("
            <> "Right" <+> toParamBinder name
            <> ")" <+> "->"
            <> PP.line
            <> PP.indent
              4
              ( continuation
              )
        )
  | otherwise =
    "case" <+> "Control.Monad.join" <+> "(" <> "fmap" <+> "(" <> "fmap" <+> "(" <> "Web.HttpApiData.parseUrlPiece" <+> "." <+> "Data.Text.Encoding.decodeUtf8" <> ")" <> ")" <+> "("
      <> "Data.List.lookup" <+> "\""
      <> toParamName name
      <> "\"" <+> "("
      <> "Network.Wai.queryString" <+> "request"
      <> ")"
      <> ")"
      <> ")" <+> "of"
      <> PP.line
      <> PP.indent
        4
        ( "Just" <+> "(" <> "Left" <+> "err" <> ")" <+> "->" <> PP.line
            <> PP.indent
              4
              ( "invalidRequest" <+> "err"
              )
            <> PP.line
            <> "_x" <+> "->"
            <> PP.line
            <> PP.indent
              4
              ( "let" <+> "!" <> toParamBinder name <+> "=" <+> "fmap" <+> "(" <> "\\"
                  <> "("
                  <> "Right" <+> "_x"
                  <> ")" <+> "->" <+> "_x"
                  <> ")" <+> "_x" <+> "in"
                  <> PP.line
                  <> PP.indent
                    4
                    ( continuation
                    )
              )
        )
