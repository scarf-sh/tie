{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Codegen.Response
  ( codegenResponses,
    codegenResponseAuxFile,
  )
where

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
  ( Operation (..),
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
  let 
      responseHeaderTypes Response {headers} =
        PP.hsep (map codegenHeaderSchema headers)

      decl =
        "data" <+> toApiResponseTypeName name <> PP.line
          <> PP.indent
            4
            ( PP.vsep $
                [ op <+> toApiResponseConstructorName name statusCode <+> case jsonResponseContent of
                    Nothing -> mempty
                    Just jsonContent ->
                      codegenFieldType jsonContent
                        <+> responseHeaderTypes response
                  | (op, (statusCode, response@Response {jsonResponseContent})) <- zip ("=" : repeat "|") responses
                ]
                  ++ [ "|" <+> toApiDefaultResponseConstructorName name
                         <+> "Network.HTTP.Types.Status"
                         <+> case jsonResponseContent of
                           Nothing -> mempty
                           Just jsonContent ->
                             codegenFieldType jsonContent
                               <+> responseHeaderTypes response
                       | Just response@Response {jsonResponseContent} <- [defaultResponse]
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
        Response {jsonResponseContent = Just {}} -> True
        _ -> False

      bodyBinder response
        | hasBody response = "x"
        | otherwise = mempty

      bodySerialize Response {jsonResponseContent}
        | Just {} <- jsonResponseContent =
          "(" <> "Data.Aeson.fromEncoding" <+> "(" <> "Data.Aeson.toEncoding" <+> "x" <> ")" <> ")"
        | otherwise =
          "mempty"

      responseHeaders Response {jsonResponseContent}
        | Just {} <- jsonResponseContent =
          "[(Network.HTTP.Types.hContentType, \"application/json\")]"
        | otherwise =
          "[]"

      responseHeaderBinders Response {headers} =
        mempty

      decl =
        "instance" <+> "ToResponse" <+> toApiResponseTypeName operationName <+> "where" <> PP.line
          <> PP.indent
            4
            ( PP.vsep $
                [ "toResponse" <+> "("
                    <> toApiResponseConstructorName operationName statusCode <+> bodyBinder response
                      <+> responseHeaderBinders response
                    <> ")"
                      <+> "="
                    <> PP.line
                    <> PP.indent
                      4
                      ( "Network.Wai.responseBuilder" <+> "(" <> "toEnum" <+> PP.pretty statusCode <> ")"
                          <+> responseHeaders response
                          <+> bodySerialize response
                      )
                  | (statusCode, response) <- responses
                ]
                  ++ [ "toResponse" <+> "(" <> toApiDefaultResponseConstructorName operationName <+> "status" <+> bodyBinder response <+> responseHeaderBinders response <> ")"
                         <+> "="
                         <> PP.line
                         <> PP.indent
                           4
                           ( "Network.Wai.responseBuilder" <+> "status"
                               <+> responseHeaders response
                               <+> bodySerialize response
                           )
                       | Just response@Response {jsonResponseContent} <- [defaultResponse]
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
