{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Tie.Type
  ( StringFormat (..),
    BasicType (..),
    ObjectType (..),
    Type (..),
    Enumeration (..),
    Name,
    Named (..),
    namedType,

    -- * Conversion from 'OpenApi.Schema' to 'Type'.
    schemaToType,
    schemaRefToType,

    -- * Accessors and operators working with 'Type'
    isBasicType,
    isEnumType,
    isArrayType,
    isObjectType,
    isOneOfType,

    -- * Normalize types

    --    normalizeObjectType,
    --    normalizeVariants,
    normalizeType,

    -- * Dependencies
    namedTypeDependencies,
    transitiveDependencies,
    typeDependencies,
  )
where

import Control.Monad.Writer (runWriterT, tell)
import qualified Data.Aeson as Aeson
import Data.Foldable (foldr1)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import qualified Data.HashSet as HashSet
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as Text
import Tie.Name (Name, fromText)
import Tie.Resolve (Resolver, resolve)
import Prelude hiding (Type)

-- | Formats a String can have
data StringFormat
  = -- | Default for when there is no format defined in the type declaration.
    FormatDefault
  | -- | Full-date notation as defined by RFC 3339, section 5.6, for example, 2017-07-21
    FormatDate
  | -- | The date-time notation as defined by RFC 3339, section 5.6, for example, 2017-07-21T17:32:28Z
    FormatDateTime
  | -- | A hint to UIs to mask the input
    FormatPassword
  | -- | base64-encoded characters, for example, U3dhZ2dlciByb2Nrcw==
    FormatByte
  | -- | binary data, used to describe files
    FormatBinary
  deriving (Eq, Ord, Show)

-- | Represents an OpenAPI enumeration.
data Enumeration = Enumeration
  { -- | The allowed values for this 'Enum'.
    alternatives :: [Text],
    -- | Whether 'null' is a valid 'Enum' value.
    includeNull :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Basic types OpenAPI data types.
data BasicType
  = TyString {format :: StringFormat}
  | TyEnum Enumeration
  | TyNumber
  | TyInteger
  | TyBoolean
  deriving (Eq, Ord, Show)

-- | An object is a collection of property/value pairs.
data ObjectType ty = ObjectType
  { properties :: HashMap Name ty,
    requiredProperties :: HashSet Name,
    freeFormObjectType :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Our own version of 'OpenApi.Referenced'.
data Named ty
  = Named Name ty
  | Unnamed ty
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

namedType :: Named ty -> ty
namedType named = case named of
  Named _ ty -> ty
  Unnamed ty -> ty

-- | This is our internal representation for 'OpenApi.Schema'. From 'Type' we derive
-- the Haskell data types as well as the serialization code.
data Type
  = -- | OpenApi's allOf
    AllOf [Named Type]
  | -- | OpenApi's anyOf
    AnyOf [Named Type]
  | -- | OpenApi's oneOf
    OneOf [Named Type]
  | -- | OpenApi's not
    Not (Named Type)
  | -- | Basic, primitive types
    Basic BasicType
  | -- | Objects and it's properties
    Object (ObjectType (Named Type))
  | -- | Arrays of elements
    Array (Named Type)
  deriving (Eq, Ord, Show)

-- | Casts a 'Type' to a 'BasicType' if possible.
isBasicType :: Type -> Maybe BasicType
isBasicType typ = case typ of
  AllOf [x] -> isBasicType (namedType x)
  AllOf {} -> Nothing
  AnyOf [x] -> isBasicType (namedType x)
  AnyOf {} -> Nothing
  OneOf [x] -> isBasicType (namedType x)
  OneOf {} -> Nothing
  Not {} -> Nothing
  Basic basicType -> Just basicType
  Object {} -> Nothing
  Array {} -> Nothing

-- | Casts a 'Type' to an 'Enumeration' if possible.
isEnumType :: Type -> Maybe Enumeration
isEnumType typ
  | Just (TyEnum enum) <- isBasicType typ =
    Just enum
  | otherwise =
    Nothing

schemaRefToType ::
  Monad m =>
  Resolver m ->
  OpenApi.Referenced OpenApi.Schema ->
  m (Named Type)
schemaRefToType resolver referencedSchema = do
  schema <- resolve resolver referencedSchema
  case referencedSchema of
    OpenApi.Ref reference ->
      Named (fromText (OpenApi.getReference reference))
        <$> schemaToType resolver schema
    OpenApi.Inline schema -> do
      Unnamed <$> schemaToType resolver schema

-- | Converts an 'OpenApi.Schema' to our internal 'Type' representation.
-- An optional 'ComponentName' indicates the name of component.
schemaToType :: Monad m => Resolver m -> OpenApi.Schema -> m Type
schemaToType resolver schema
  | Just allOfsRefs <- OpenApi._schemaAllOf schema = do
    AllOf <$> traverse (schemaRefToType resolver) allOfsRefs
  | Just oneOfsRefs <- OpenApi._schemaOneOf schema =
    OneOf <$> traverse (schemaRefToType resolver) oneOfsRefs
  | Just anyOfRefs <- OpenApi._schemaAnyOf schema =
    AnyOf <$> traverse (schemaRefToType resolver) anyOfRefs
  | Just notOfRef <- OpenApi._schemaNot schema =
    Not <$> schemaRefToType resolver notOfRef
  | Just schemaType <- OpenApi._schemaType schema =
    case schemaType of
      OpenApi.OpenApiString ->
        pure (Basic (schemaToStringyType schema))
      OpenApi.OpenApiNumber ->
        pure (Basic TyNumber)
      OpenApi.OpenApiInteger ->
        pure (Basic TyInteger)
      OpenApi.OpenApiBoolean ->
        pure (Basic TyBoolean)
      OpenApi.OpenApiArray
        | Just items <- OpenApi._schemaItems schema ->
          case items of
            OpenApi.OpenApiItemsObject itemsSchemaRef ->
              Array <$> schemaRefToType resolver itemsSchemaRef
            OpenApi.OpenApiItemsArray _itemsSchemaRefs ->
              undefined -- TODO find out what tuple schemas are
        | otherwise ->
          undefined -- TODO array type without items
      OpenApi.OpenApiNull ->
        undefined -- TODO need a BasicType for that
      OpenApi.OpenApiObject ->
        Object <$> schemaToObjectType resolver schema
  -- Heuristic: if the 'OpenApi.Schema' has properties attached
  -- treat it as object.
  | not (InsOrd.null (OpenApi._schemaProperties schema)) =
    Object <$> schemaToObjectType resolver schema
  -- It's an enum but without explicit "type: string"
  | Just _enum <- OpenApi._schemaEnum schema =
    pure (Basic (schemaToStringyType schema))
  | otherwise =
    traceShow schema undefined

-- | Resolves an 'OpenApi.Schema' to an 'ObjectType'. In case the the 'OpenApi.Schema' is an
-- allOf-schema. This function doesn't do any additional type checking.
schemaToObjectType ::
  Monad m =>
  Resolver m ->
  OpenApi.Schema ->
  m (ObjectType (Named Type))
schemaToObjectType resolver schema = do
  properties <-
    traverse
      (schemaRefToType resolver)
      (InsOrd.toHashMap (OpenApi._schemaProperties schema))
  freeFormObjectType <- case OpenApi._schemaAdditionalProperties schema of
    Nothing -> pure False
    Just (OpenApi.AdditionalPropertiesAllowed allowed) -> pure allowed
    Just (OpenApi.AdditionalPropertiesSchema schema) -> undefined -- TODO what exactly is this?
  pure $
    ObjectType
      { freeFormObjectType,
        properties = HashMap.mapKeys fromText properties,
        requiredProperties =
          HashSet.fromList (map fromText (OpenApi._schemaRequired schema))
      }

-- | Treat an 'OpenApi.Schema' as stringy. Accounts for enumerations
-- as well. This function doesn't do any additional type checking.
schemaToStringyType :: OpenApi.Schema -> BasicType
schemaToStringyType schema
  | Just enum <- OpenApi._schemaEnum schema = do
    TyEnum $
      Enumeration
        { alternatives = [alt | Aeson.String alt <- enum],
          includeNull = Aeson.Null `elem` enum
        }
  | otherwise =
    TyString
      { format = FormatDefault -- TODO
      }

-- | Extracts the shallow dependencies of a 'Type' by traversing the 'Type' and
-- until we hit a 'Named' type.
typeDependencies :: (Named Type -> [Name]) -> Type -> [Name]
typeDependencies getDependencies ty =
  -- For allOf, anyOf, oneOf look through all the dependencies - similar to what
  -- we do in 'isObjectType'.
  case ty of
    AllOf allOfs ->
      concatMap getDependencies allOfs
    AnyOf anyOfs ->
      concatMap getDependencies anyOfs
    OneOf oneOfs ->
      concatMap getDependencies oneOfs
    Not not ->
      typeDependencies getDependencies (namedType not)
    Basic {} ->
      []
    Object objectType ->
      objectTypeDependencies getDependencies objectType
    Array elemType ->
      getDependencies elemType

-- | Dependencies of a 'Named Type'. This doesn't return transitive
-- dependencies.
namedTypeDependencies :: Named Type -> [Name]
namedTypeDependencies named = case named of
  Named name _elem ->
    -- This is key: We don't want to descend into elem's type.
    [name]
  Unnamed elem ->
    typeDependencies namedTypeDependencies elem

-- | Extract all the transitive dependencies form a 'Named Type'.
transitiveDependencies :: Named Type -> [Name]
transitiveDependencies named = case named of
  Named name typ ->
    name : typeDependencies transitiveDependencies typ
  Unnamed typ ->
    typeDependencies transitiveDependencies typ

-- | Dependencies of an 'ObjectType'.
objectTypeDependencies :: (Named Type -> [Name]) -> ObjectType (Named Type) -> [Name]
objectTypeDependencies getDependencies objectType =
  concatMap getDependencies (toList (properties objectType))

-- | Casting a 'Type' to the set of types it could be.
isOneOfType :: Type -> Maybe [Named Type]
isOneOfType ty = case ty of
  OneOf [_] ->
    Nothing
  OneOf oneOfs ->
    Just oneOfs
  _ ->
    Nothing

isArrayType :: Type -> Maybe (Named Type)
isArrayType ty = case ty of
  Array elem -> Just elem
  _ -> Nothing

-- | Casting a 'Type' to an 'ObjectType', if possible. `isObjectType` looks through
-- allOf, oneOf, anyOf to ensure
isObjectType :: Type -> Maybe (ObjectType (Named Type))
isObjectType ty = case ty of
  AllOf allOfs -> do
    -- We have the choice: we could use traverse instead and fail
    -- everything in case any subtype is not an Object. For now,
    -- we ignore "type errors" and collect everything we get. I
    -- found other code generators for OpenApi that behave that way.
    let objects = catMaybes (map (isObjectType . namedType) allOfs)
    pure (combine objects)
  AnyOf anyOfs -> do
    -- Look through all the objects, mark all of the properties optional
    let objects =
          [ object {requiredProperties = HashSet.empty}
            | object <- catMaybes (map (isObjectType . namedType) anyOfs)
          ]
    pure (combine objects)
  OneOf oneOfs -> do
    -- Look through all the objects, mark all of the properties optional
    let objects =
          [ object {requiredProperties = HashSet.empty}
            | object <- catMaybes (map (isObjectType . namedType) oneOfs)
          ]
    pure (combine objects)
  Not {} -> Nothing
  Basic {} -> Nothing
  Object obj -> Just obj
  Array {} -> Nothing
  where
    -- In principle this is Semigroup and Monoid instance. But it's too
    -- early in the design to rely on that.
    combine xs = case nonEmpty xs of
      Nothing -> emptyObject
      Just os -> foldr1 combineObjects os -- TODO make strict
    emptyObject =
      ObjectType
        { properties = mempty,
          requiredProperties = mempty,
          freeFormObjectType = False
        }

    -- Combine two ObjectTypes. Doesn't report common fields!
    combineObjects o1 o2 =
      ObjectType
        { properties = properties o1 <> properties o2,
          requiredProperties = requiredProperties o1 <> requiredProperties o2,
          freeFormObjectType = freeFormObjectType o1 || freeFormObjectType o2
        }

normalizeNamedType ::
  Monad m =>
  -- | Generate a new name based on the context of an anonymous type. Within 'normalizeNamedType'
  -- we don't know anything about the enclosing context and we expect the callers to do the right
  -- thing (tm).
  m Name ->
  -- | Named type to normalize.
  Named Type ->
  m (Named Type, [(Name, Type)])
normalizeNamedType assignName namedType = case namedType of
  Named {} ->
    pure (namedType, [])
  Unnamed typ
    | Just enum <- isEnumType typ -> do
      newTypeName <- assignName
      pure
        (Named newTypeName (Basic (TyEnum enum)), [(newTypeName, Basic (TyEnum enum))])
    | Just variants <- isOneOfType typ -> do
      newTypeName <- assignName
      pure (Named newTypeName (OneOf variants), [(newTypeName, OneOf variants)])
    | Just objectType <- isObjectType typ -> do
      newTypeName <- assignName
      pure (Named newTypeName (Object objectType), [(newTypeName, Object objectType)])
    | Just (Unnamed elemType) <- isArrayType typ -> do
      (normedElemType, inlineDefinitions) <-
        normalizeNamedType assignName (Unnamed elemType)
      pure (Unnamed (Array normedElemType), inlineDefinitions)
    | otherwise ->
      pure (namedType, [])

normalizeObjectType ::
  Monad m =>
  -- | Assign a name to an anonnymous type in a field of an 'ObjectType'
  (Name -> m Name) ->
  -- | 'ObjectType' to normalize
  ObjectType (Named Type) ->
  m (ObjectType (Named Type), [(Name, Type)])
normalizeObjectType assignObjectFieldTypeName objectType@ObjectType {..} = do
  (properties, newTypes) <- runWriterT $
    flip HashMap.traverseWithKey properties $ \fieldName fieldType -> do
      (normedType, inlineDefinitions) <-
        lift $
          normalizeNamedType (assignObjectFieldTypeName fieldName) fieldType
      tell inlineDefinitions
      pure normedType
  pure (objectType {properties}, newTypes)

normalizeVariants ::
  Monad m =>
  (Int -> m Name) ->
  [Named Type] ->
  m ([Named Type], [(Name, Type)])
normalizeVariants assignName variants = runWriterT $
  forM (zip [1 ..] variants) $ \(i, variant) -> do
    (normedVariant, inlineDefinitions) <-
      lift $ normalizeNamedType (assignName i) variant
    tell inlineDefinitions
    pure normedVariant

-- | Normalizes a 'Type' by assigning each anonymous, inline definition a name.
-- Returns the normalized 'Type' alongside with the additional inline definitions.
--
-- `normalizeTypeShallow` doesn't recurse into the tree but only normalizes the
-- direct references to anonymous definitions. See `normalizeType` for the recursion.
-- Otherwise handling of Arrays and Enums gets tricky.
--
-- This is called on things that already have a name like
--   - top-level definitions
--   - inline definitions that we just assigned a name
normalizeTypeShallow ::
  Monad m =>
  -- | Assign a name to an anonnymous type in a field of an 'ObjectType'
  (Name -> Name -> m Name) ->
  -- | Assign a name to an anonnymous type in the ith constructor of a
  -- variant type
  (Name -> Int -> m Name) ->
  -- | Name of the type to normalize
  Name ->
  -- | Type to normalize
  Type ->
  m (Type, [(Name, Type)])
normalizeTypeShallow
  assignObjectFieldTypeName
  assignOneOfTypeName
  typeName
  typ
    | Just variants <- isOneOfType typ = do
      (variants, inlineDefinitions) <-
        normalizeVariants (assignOneOfTypeName typeName) variants
      pure (OneOf variants, inlineDefinitions)
    | Just objectType <- isObjectType typ = do
      (objectType, inlineDefinitions) <-
        normalizeObjectType (assignObjectFieldTypeName typeName) objectType
      pure (Object objectType, inlineDefinitions)
    -- There is no need to handle Enums and Arrays here. Remember this is
    -- only called on types that already have names.
    | otherwise =
      pure (typ, [])

-- Normalizes a 'Type' by assigning each anonymous, inline definition a name.
-- Returns the normalized 'Type' alongside with the additional inline definitions.
normalizeType ::
  Monad m =>
  -- | Assign a name to an anonnymous type in a field of an 'ObjectType'
  (Name -> Name -> m Name) ->
  -- | Assign a name to an anonnymous type in the ith constructor of a
  -- variant type
  (Name -> Int -> m Name) ->
  -- | Name of the type to normalize
  Name ->
  -- | Type to normalize
  Type ->
  m (Type, [(Name, Type)])
normalizeType
  assignObjectFieldTypeName
  assignOneOfTypeName
  typeName
  typ = do
    -- Normalize the type.
    (normedType, inlineDefinitions) <-
      normalizeTypeShallow
        assignObjectFieldTypeName
        assignOneOfTypeName
        typeName
        typ
    -- Now, normalize the inline definitions recursively to ensure
    -- every anonymous definition in the tree has a name assigned.
    normalizedInlineDefinitions <-
      foldMapM
        ( \(inlineDefName, inlineDefType) -> do
            (normedInlineDefType, moreInlineDefinitions) <-
              normalizeType
                assignObjectFieldTypeName
                assignOneOfTypeName
                inlineDefName
                inlineDefType
            pure
              ((inlineDefName, normedInlineDefType) : moreInlineDefinitions)
        )
        inlineDefinitions
    pure (normedType, normalizedInlineDefinitions)
