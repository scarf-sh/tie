{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Resolve
  ( Resolvable,
    Resolver,
    newResolver,
    resolve,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as Text

-- | Resolve an 'OpenApi.Reference' to the underlying component.
newtype Resolver m = Resolver
  { resolve :: forall a. (Resolvable a) => OpenApi.Referenced a -> m a
  }

newResolver ::
  (Applicative m) =>
  OpenApi.Components ->
  (forall a. OpenApi.Reference -> m a) ->
  Resolver m
newResolver components notFound =
  Resolver (resolveComponent components notFound)

resolveComponent ::
  (Applicative m, Resolvable a) =>
  -- | Inventory of components we can resolve to
  OpenApi.Components ->
  -- | What to do in case a 'OpenApi.Reference' is not found
  (OpenApi.Reference -> m a) ->
  -- | 'OpenApi.Reference' to resolve
  OpenApi.Referenced a ->
  m a
resolveComponent components notFound = \referenced -> do
  case referenced of
    OpenApi.Inline a ->
      pure a
    OpenApi.Ref reference
      | Just a <-
          InsOrd.lookup
            (OpenApi.getReference reference)
            (resolvables components) ->
          pure a
      | otherwise -> do
          notFound reference

-- | Helper class helping to dispatch from 'OpenApi.Referenced' to component type @a@.
class Resolvable a where
  -- | Resolves the `OpenApi.Components` to the given corresponding `Definitions`.
  resolvables ::
    OpenApi.Components ->
    OpenApi.Definitions a

instance Resolvable OpenApi.Schema where
  resolvables = OpenApi._componentsSchemas

instance Resolvable OpenApi.Response where
  resolvables = OpenApi._componentsResponses

instance Resolvable OpenApi.Param where
  resolvables = OpenApi._componentsParameters

instance Resolvable OpenApi.Example where
  resolvables = OpenApi._componentsExamples

instance Resolvable OpenApi.RequestBody where
  resolvables = OpenApi._componentsRequestBodies

instance Resolvable OpenApi.Header where
  resolvables = OpenApi._componentsHeaders
