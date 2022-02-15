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
import Tie.Codegen.Schema (codegenFieldType, codegenParamSchema)
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
  let decl =
        "data" <+> toApiResponseTypeName name <> PP.line
          <> PP.indent
            4
            ( PP.vsep $
                [ op <+> toApiResponseConstructorName name statusCode <+> codegenFieldType jsonResponseContent
                  | (op, (statusCode, Response {jsonResponseContent})) <- zip ("=" : repeat "|") responses
                ]
                  ++ [ "|" <+> toApiDefaultResponseConstructorName name
                         <+> "Network.HTTP.Types.Status"
                         <+> codegenFieldType jsonResponseContent
                       | Just Response {jsonResponseContent} <- [defaultResponse]
                     ]
            )
      instances =
        codegenToResponses name responses defaultResponse

  pure (PP.vsep [decl, mempty, instances])

codegenToResponses :: Name -> [(Int, Response)] -> Maybe Response -> Doc ann
codegenToResponses operationName responses defaultResponse =
  let decl =
        "instance" <+> "ToResponse" <+> toApiResponseTypeName operationName <+> "where" <> PP.line
          <> PP.indent
            4
            ( PP.vsep $
                [ "toResponse" <+> "(" <> toApiResponseConstructorName operationName statusCode <+> "x" <> ")"
                    <+> "="
                    <> PP.line
                    <> PP.indent
                      4
                      ( "Network.Wai.responseBuilder" <+> "(" <> "toEnum" <+> PP.pretty statusCode <> ")"
                          <+> "[(Network.HTTP.Types.hContentType, \"application/json\")]"
                          <+> "(" <> "Data.Aeson.fromEncoding"
                          <+> "(" <> "Data.Aeson.toEncoding"
                          <+> "x" <> ")" <> ")"
                      )
                  | (statusCode, _response) <- responses
                ]
                  ++ [ "toResponse" <+> "(" <> toApiDefaultResponseConstructorName operationName <+> "status" <+> "x" <> ")"
                         <+> "="
                         <> PP.line
                         <> PP.indent
                           4
                           ( "Network.Wai.responseBuilder" <+> "status"
                               <+> "[(Network.HTTP.Types.hContentType, \"application/json\")]"
                               <+> "(" <> "Data.Aeson.fromEncoding"
                               <+> "(" <> "Data.Aeson.toEncoding"
                               <+> "x" <> ")" <> ")"
                           )
                       | Just Response {jsonResponseContent} <- [defaultResponse]
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
