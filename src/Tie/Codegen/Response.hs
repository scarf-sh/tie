{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Tie.Codegen.Response
  ( codegenResponses,
    codegenResponseAuxFile,
  )
where

import qualified Data.ByteString as ByteString
import Data.List (lookup)
import qualified Data.Text as Text
import Network.HTTP.Media (renderHeader)
import Paths_tie (getDataFileName)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import System.IO.Unsafe (unsafePerformIO)
import Tie.Codegen.Schema
  ( codegenFieldType,
    codegenHeaderSchema,
    codegenParamSchema,
  )
import Tie.Name
  ( Name,
    toApiDefaultResponseConstructorName,
    toApiMemberName,
    toApiResponseConstructorName,
    toApiResponseTypeName,
    toParamBinder,
    toParamName,
  )
import Tie.Operation
  ( Header (..),
    Operation (..),
    Param (..),
    Path,
    PathSegment (..),
    RequestBody (..),
    Response (..),
  )
import Tie.Resolve (Resolver)

-- | Generate code for the responses of an 'Operation'.
codegenResponses ::
  Monad m =>
  Resolver m ->
  -- | Aux. Response module name TODO make this a proper type
  Text ->
  Operation ->
  m (Doc ann)
codegenResponses resolver responseModuleName Operation {..} = do
  let responseBodyType Response {responseContent}
        -- We treat JSON responses specially
        | Just jsonContent <- lookup "application/json" responseContent =
            [maybe "Data.Aeson.Value" codegenFieldType jsonContent]
        | Just jsonLdContent <- lookup "application/x-ndjson" responseContent =
            ["(" <> PP.pretty responseModuleName <> "." <> "NDJSON" <+> maybe "Data.Aeson.Value" codegenFieldType jsonLdContent <> ")"]
        -- Everything else we use a Network.Wai.StreamingBody type
        | not (null responseContent) =
            ["Network.Wai.StreamingBody"]
        -- Otherwise, no response body present
        | otherwise =
            []

      responseHeaderTypes Response {headers} =
        map codegenHeaderSchema headers

      -- Since we insert StreamingBody for mime types that we don't know,
      -- we have to generate Show instances for those types!
      canDeriveStockShowInstanceForResponse Response {responseContent}
        | Just _ <- lookup "application/json" responseContent =
            True
        | Just _ <- lookup "application/x-ndjson" responseContent =
            False
        | not (null responseContent) =
            False
        | otherwise =
            True

      requiresCustomShowInstance =
        not $
          all
            canDeriveStockShowInstanceForResponse
            (maybeToList defaultResponse ++ map snd responses)

      decl =
        "data"
          <+> toApiResponseTypeName name
            <> PP.line
            <> PP.indent
              4
              ( PP.vsep $
                  [ PP.hsep $
                      concat
                        [ [op, toApiResponseConstructorName name statusCode],
                          responseBodyType response,
                          responseHeaderTypes response
                        ]
                    | (op, (statusCode, response)) <- zip ("=" : repeat "|") responses
                  ]
                    ++ [ PP.hsep $
                           concat
                             [ ["|", toApiDefaultResponseConstructorName name, "Network.HTTP.Types.Status"],
                               responseBodyType response,
                               responseHeaderTypes response
                             ]
                         | Just response <- [defaultResponse]
                       ]
                    ++ [ "deriving" <+> "(" <> "Show" <> ")"
                         | not requiresCustomShowInstance
                       ]
              )

      instances =
        codegenToResponses responseModuleName name responses defaultResponse

      showInstance =
        "instance"
          <+> "Show"
          <+> toApiResponseTypeName name
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( "show" <+> "_" <+> "=" <+> "\"" <> toApiResponseTypeName name <+> "{}" <> "\""
              )

  pure
    ( PP.vsep $
        intersperse mempty $
          [decl, instances] ++ [showInstance | requiresCustomShowInstance]
    )

codegenToResponses ::
  -- | Aux. Response module name TODO make this a proper type
  Text ->
  Name ->
  [(Int, Response)] ->
  Maybe Response ->
  Doc ann
codegenToResponses responseModuleName operationName responses defaultResponse =
  let hasBody response = case response of
        Response {responseContent = _ : _} -> True
        _ -> False

      bodyBinder response
        | hasBody response = ["x"]
        | otherwise = []

      waiResponse Response {responseContent}
        | Just _ <- lookup "application/json" responseContent =
            -- JSON is very easy to turn into Builders!
            "Network.Wai.responseBuilder"
        | Just _ <- lookup "application/x-ndjson" responseContent =
            PP.pretty responseModuleName <> "." <> "responseNDJSON"
        | not (null responseContent) =
            -- Tie doesn't know about the content type of this response,
            -- uses a Stream instaed
            "Network.Wai.responseStream"
        | otherwise =
            -- For empty response bodies we pass mempty
            "Network.Wai.responseBuilder"

      bodySerialize Response {responseContent}
        | Just _ <- lookup "application/json" responseContent =
            "(" <> "Data.Aeson.fromEncoding" <+> "(" <> "Data.Aeson.toEncoding" <+> "x" <> ")" <> ")"
        | Just _ <- lookup "application/x-ndjson" responseContent =
            "x"
        | not (null responseContent) =
            "x"
        | otherwise =
            "mempty"

      responseHeaders response@Response {responseContent, headers} =
        let contentType
              | Just _ <- lookup "application/json" responseContent =
                  ["(Network.HTTP.Types.hContentType, \"application/json\")"]
              | Just _ <- lookup "application/x-ndjson" responseContent =
                  ["(Network.HTTP.Types.hContentType, \"application/x-ndjson\")"]
              | (unknownMediaType, _) : _ <- responseContent =
                  ["(Network.HTTP.Types.hContentType, \"" <> PP.pretty @Text (decodeUtf8 (renderHeader unknownMediaType)) <> "\")"]
              | otherwise =
                  []

            requiredHeaders =
              [ "(\"" <> toParamName name <> "\"," <+> "Web.HttpApiData.toHeader" <+> toParamBinder name <> ")"
                | Header {name, required = True} <- headers
              ]

            optionalHeaders =
              [ "[" <> "(\"" <> toParamName name <> "\","
                  <+> "Web.HttpApiData.toHeader"
                  <+> toParamBinder name <> ")"
                  <+> "|"
                  <+> "Just"
                  <+> toParamBinder name
                  <+> "<-"
                  <+> "[" <> toParamBinder name <> "]" <> "]"
                | Header {name, required = False} <- headers
              ]
         in "("
              <> PP.concatWith
                (\x y -> x <+> "++" <+> y)
                ( optionalHeaders
                    ++ [ "["
                           <> PP.concatWith
                             (\x y -> x <> "," <+> y)
                             (contentType ++ requiredHeaders)
                           <> "]"
                       ]
                )
              <> ")"

      decl =
        "instance"
          <+> "ToResponse"
          <+> toApiResponseTypeName operationName
          <+> "where"
            <> PP.line
            <> PP.indent
              4
              ( PP.vsep $
                  [ "toResponse"
                      <+> "("
                        <> PP.hsep
                          ( concat
                              [ [toApiResponseConstructorName operationName statusCode],
                                bodyBinder response,
                                [toParamBinder name | Header {name} <- headers]
                              ]
                          )
                        <> ")"
                      <+> "="
                        <> PP.line
                        <> PP.indent
                          4
                          ( waiResponse response
                              <+> "Network.HTTP.Types.status" <> PP.pretty statusCode
                              <+> responseHeaders response
                              <+> bodySerialize response
                          )
                    | (statusCode, response@Response {headers}) <- responses
                  ]
                    ++ [ "toResponse"
                           <+> "("
                             <> PP.hsep
                               ( concat
                                   [ [toApiDefaultResponseConstructorName operationName, "status"],
                                     bodyBinder response,
                                     [toParamBinder name | Header {name} <- headers]
                                   ]
                               )
                             <> ")"
                           <+> "="
                             <> PP.line
                             <> PP.indent
                               4
                               ( waiResponse response
                                   <+> "status"
                                   <+> responseHeaders response
                                   <+> bodySerialize response
                               )
                         | Just response@Response {headers} <- [defaultResponse]
                       ]
              )
   in decl

auxTemplate :: Text
auxTemplate = unsafePerformIO $ do
  file <- getDataFileName "Response.template.hs"
  contents <- ByteString.readFile file
  pure (decodeUtf8 contents)
{-# NOINLINE auxTemplate #-}

codegenResponseAuxFile ::
  -- | Module name
  Text ->
  Text
codegenResponseAuxFile moduleName =
  Text.replace "Tie.Template.Response_" moduleName auxTemplate
