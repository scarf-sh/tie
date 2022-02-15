{-# LANGUAGE OverloadedStrings #-}

module Test.Tie.Operation where

import Test.Hspec (Spec, context, it, shouldBe)
import Test.Tasty.Hspec
import Tie.Operation (PathSegment (..), parsePath)

spec_parsePath :: Spec
spec_parsePath = do
  it "parses /users/create" $
    parsePath "/users/create" `shouldBe` [StaticSegment "users", StaticSegment "create"]
  it "parses /users/{id}" $
    parsePath "/users/{id}" `shouldBe` [StaticSegment "users", VariableSegment "id"]
  it "parses /users/{id}/address" $
    parsePath "/users/{id}/address" `shouldBe` [StaticSegment "users", VariableSegment "id", StaticSegment "address"]
