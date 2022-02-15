{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Codegen.Operation
  ( codegenOperation,
    codegenOperations,
  )
where

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
  dataApiDecl <- codegenApiType resolver operations
  operationsCode <- traverse (codegenOperation resolver) operations
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
                        <> PP.line
                        <> "invalidRequest" <+> "_" <+> "="
                        <> PP.line
                        <> PP.indent
                          4
                          ( "respond" <+> "(" <> "Network.Wai.responseBuilder"
                              <+> "(" <> "toEnum"
                              <+> "401" <> ")"
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
    sequence
      [ codegenParamSchema name schema
        | VariableSegment Param {name, schema} <- path
      ]
  pure $
    toApiMemberName name <+> "::"
      <+> PP.concatWith
        (\x y -> x <+> "->" <+> y)
        ( paramsCode
            ++ [ codegenFieldType jsonRequestBodyContent
                 | Just RequestBody {jsonRequestBodyContent} <- [requestBody]
               ]
            ++ ["m" <+> toApiResponseTypeName name]
        )

codegenOperation :: Monad m => Resolver m -> Operation -> m (PP.Doc ann)
codegenOperation resolver operation@Operation {..} = do
  pure $
    codegenPathGuard path $
      codegenMethodGuard
        [ ( method,
            codegenRequestBodyGuard
              requestBody
              ( codegenCallApiMember name path requestBody
              )
          )
        ]

codegenCallApiMember :: Name -> Path -> Maybe RequestBody -> PP.Doc ann
codegenCallApiMember operationName path requestBody =
  "run" <+> "request" <+> "$" <+> "do" <> PP.line
    <> PP.indent
      4
      ( "response" <+> "<-" <+> toApiMemberName operationName
          <+> "api"
          <+> PP.hsep [toParamBinder name | VariableSegment Param {name} <- path]
          <+> (maybe mempty (\_ -> "body") requestBody)
            <> PP.line
            <> "Control.Monad.IO.Class.liftIO"
          <+> "(" <> "respond"
          <+> "(" <> "toResponse"
          <+> "response" <> ")" <> ")"
      )

-- | Codegen a 'PathSegment'.
codegenSegment :: Monad m => Resolver m -> PathSegment Param -> m (PP.Doc ann)
codegenSegment _resolver segment = case segment of
  StaticSegment literal ->
    pure ("\"" <> PP.pretty literal <> "\"")
  VariableSegment Param {name, schema} -> do
    code <- codegenParamSchema name schema
    let capture =
          "Capture" <+> "\"" <> toParamName name <> "\"" <+> code
    pure capture

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
              ( "Left" <+> "_err" <+> "->" <+> "undefined" <> PP.line
                  <> "Right" <+> "bodyValue" <+> "->"
                  <> PP.line
                  <> PP.indent
                    4
                    ( "case" <+> "Data.Aeson.Types.parseEither" <+> "Data.Aeson.parseJSON" <+> "bodyValue" <+> "of" <> PP.line
                        <> PP.indent
                          4
                          ( "Left" <+> "_err" <+> "->" <+> "undefined" <> PP.line
                              <> "Right" <+> "body" <+> "->"
                              <> PP.line
                              <> PP.indent 4 continuation
                          )
                    )
              )
        )
