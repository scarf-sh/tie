{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Codegen.Response
  ( codegenResponses,
    codegenResponseAuxFile,
  )
where

import Data.List (lookup)
import Prettyprinter (Doc, (<+>))
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
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
codegenResponses :: Monad m => Resolver m -> Operation -> m (Doc ann)
codegenResponses resolver Operation {..} = do
  let responseBodyType Response {responseContent}
        | Just jsonContent <- lookup "application/json" responseContent =
          [codegenFieldType jsonContent]
        | otherwise =
          []

      responseHeaderTypes Response {headers} =
        map codegenHeaderSchema headers

      decl =
        "data" <+> toApiResponseTypeName name <> PP.line
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
                     ]
            )
      instances =
        codegenToResponses name responses defaultResponse

      exceptionInstance =
        "instance" <+> "Control.Exception.Exception" <+> toApiResponseTypeName name

  pure (PP.vsep [decl, mempty, exceptionInstance, mempty, instances])

codegenToResponses :: Name -> [(Int, Response)] -> Maybe Response -> Doc ann
codegenToResponses operationName responses defaultResponse =
  let hasBody response = case response of
        Response {responseContent = _ : _} -> True
        _ -> False

      bodyBinder response
        | hasBody response = ["x"]
        | otherwise = []

      bodySerialize Response {responseContent}
        | Just _ <- lookup "application/json" responseContent =
          "(" <> "Data.Aeson.fromEncoding" <+> "(" <> "Data.Aeson.toEncoding" <+> "x" <> ")" <> ")"
        | otherwise =
          "mempty"

      responseHeaders response@Response {responseContent, headers} =
        let contentType
              | Just _ <- lookup "application/json" responseContent =
                ["(Network.HTTP.Types.hContentType, \"application/json\")"]
              | otherwise =
                []

            requiredHeaders =
              [ "(\"" <> toParamName name <> "\"," <+> "Web.HttpApiData.toHeader" <+> toParamBinder name <> ")"
                | Header {name, required = True} <- headers
              ]

            optionalHeaders =
              [ "[" <> "(\"" <> toParamName name <> "\"," <+> "Web.HttpApiData.toHeader" <+> toParamBinder name <> ")"
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
        "instance" <+> "ToResponse" <+> toApiResponseTypeName operationName <+> "where" <> PP.line
          <> PP.indent
            4
            ( PP.vsep $
                [ "toResponse" <+> "("
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
                      ( "Network.Wai.responseBuilder" <+> "(" <> "toEnum" <+> PP.pretty statusCode <> ")"
                          <+> responseHeaders response
                          <+> bodySerialize response
                      )
                  | (statusCode, response@Response {headers}) <- responses
                ]
                  ++ [ "toResponse" <+> "("
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
                           ( "Network.Wai.responseBuilder" <+> "status"
                               <+> responseHeaders response
                               <+> bodySerialize response
                           )
                       | Just response@Response {headers} <- [defaultResponse]
                     ]
            )
   in decl

codegenResponseAuxFile :: Doc ann
codegenResponseAuxFile =
  "class"
    <+> "ToResponse"
    <+> "a"
    <+> "where"
      <> PP.line
      <> PP.indent
        4
        ( "toResponse" <+> "::" <+> "a" <+> "->" <+> "Network.Wai.Response"
        )
