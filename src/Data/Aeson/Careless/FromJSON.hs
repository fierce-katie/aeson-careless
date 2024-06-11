module Data.Aeson.Careless.FromJSON
  ( FromJSONWithWarnings(..)
  , GFromJSONWithWarnings(..)
  , genericParseJSONWithWarnings
  , fromJSONWithWarnings
  , fromJSONWithWarningsMaybe) where

import Control.Applicative
import Control.Lens hiding (to, ix)
import Control.Monad
import Control.Monad.Writer
import Data.Aeson
import Data.Aeson.Careless.Types
import Data.Aeson.Key as Key
import Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Aeson.Types.FromJSON
import Data.Aeson.Types.Generic
import Data.Bits (unsafeShiftR)
import Data.Either
import Data.List.NonEmpty as NE
import Data.Foldable as F
import Data.Tagged hiding (proxy)
import Data.Text as T
import Data.Vector qualified as V hiding ((++), sequence)
import GHC.Generics
import Prelude as P

-- FromJSONWithWarnings

-- A class allowing to parse JSON values paritally and collect warnings instead
-- of failing.
-- If you want your data to be parsed partially, either replace [a], NonEmpty a
-- and Maybe a with ListW a, NonEmptyW a and MaybeW a or write custom instances for
-- FromJSONWithWarnings. Otherwise, it will work just like FromJSON.
-- E.g. if you want MyResponse to be parsed with warnings, you should do smth like:
-- type MyResponse = WithWarnings MyResponse'
-- data MyResponse = MyResponse { foo :: Text, bar :: ListW Int }
-- instance FromJSON MyResponse where
--   parseJSON = parseJSONWithWarnings @MyResponse'
class FromJSONWithWarnings a where
  parseJSONWithWarnings :: Value -> Parser (WithWarnings a)

-- Like FromJSON, no warnings, we either succeeed or fail.
instance {-# OVERLAPPABLE #-} FromJSON a => FromJSONWithWarnings a where
  parseJSONWithWarnings v = pure <$> parseJSON v

-- Instead of failing, return an empty list and add a warning. Nested warnings
-- are aggregated.
instance FromJSONWithWarnings a => FromJSONWithWarnings (ListW a) where
  parseJSONWithWarnings = pure . \case
    Array (F.toList -> vs) -> let
      (errs, res) = bimap (fmap T.pack) sequence . partitionEithers . fmap
        (\(i, v) -> parseEither (\v' -> parseJSONWithWarnings v' <?> Index i) v) $
          P.zip [0..] vs
      in censor (errs <>) (ListW <$> res)
    v -> mWarn $ "Not a list: " <> T.pack (show v)

-- Parse all the list values that we can and fail only if the result is empty.
-- Warnings are aggregated.
instance FromJSONWithWarnings a => FromJSONWithWarnings (NonEmptyW a) where
  parseJSONWithWarnings v = do
    (ListW l, ws) <- runWriter <$> parseJSONWithWarnings @(ListW a) v
    maybe (fail "Empty list") (pure . writer . (,ws) . NonEmptyW) $ nonEmpty l

-- If value is not null, but can't be parsed, return Nothing and add a warning.
instance FromJSONWithWarnings a => FromJSONWithWarnings (MaybeW a) where
  parseJSONWithWarnings = pure . \case
    Null -> emptyResp
    v -> either (mWarn . T.pack) (fmap pure) . parseEither
     parseJSONWithWarnings $ v

fromJSONWithWarnings :: FromJSONWithWarnings a =>
  Value -> Either String (WithWarnings a)
fromJSONWithWarnings = parseEither parseJSONWithWarnings

fromJSONWithWarningsMaybe :: FromJSONWithWarnings a =>
  Value -> Maybe (WithWarnings a)
fromJSONWithWarningsMaybe = preview _Right . fromJSONWithWarnings

-- Generic

class GFromJSONWithWarnings f where
  gParseJSONWithWarnings :: Options -> Value -> Parser (WithWarnings (f a))

genericParseJSONWithWarnings :: (Generic a, GFromJSONWithWarnings (Rep a))
  => Options -> Value -> Parser (WithWarnings a)
genericParseJSONWithWarnings opts = fmap (fmap to) . gParseJSONWithWarnings opts

-- empty type
instance GFromJSONWithWarnings V1 where
  gParseJSONWithWarnings _ _ = fail "Attempted to parse empty type"

-- metadata
instance {-# OVERLAPPABLE #-} (GFromJSONWithWarnings a)
  => GFromJSONWithWarnings (M1 i c a) where
  gParseJSONWithWarnings opts = fmap (fmap M1) . gParseJSONWithWarnings opts

-- Parsing single fields

-- constant
instance FromJSONWithWarnings a => GFromJSONWithWarnings (K1 i a) where
  gParseJSONWithWarnings _ = fmap (fmap K1) . parseJSONWithWarnings

-- metadata
instance (GFromJSONWithWarnings' a, Datatype d)
  => GFromJSONWithWarnings (D1 d a) where
  gParseJSONWithWarnings opts = fmap (fmap M1) . gParseJSONWithWarnings'
    (tname :* opts)
    where
      tname = moduleName proxy ++ "." ++ datatypeName proxy
      proxy = undefined :: M1 _i d _f _p

-- GFromJSONWithWarnings' used after unwrapping D1

class GFromJSONWithWarnings' f where
  gParseJSONWithWarnings' :: TypeName :* Options -> Value -> Parser (WithWarnings (f a))

-- empty type
instance GFromJSONWithWarnings' V1 where
    gParseJSONWithWarnings' _ _ = fail "Attempted to parse empty type"

-- single constructor
instance (ConsFromJSON a, AllNullary (C1 c a) allNullary,
  ParseSum (C1 c a) allNullary, Constructor c)
  => GFromJSONWithWarnings' (C1 c a) where
  gParseJSONWithWarnings' p@(_ :* opts)
      | tagSingleConstructors opts
        = (unTagged :: Tagged allNullary (Parser (WithWarnings (C1 c a p))) ->
          Parser (WithWarnings (C1 c a p))) . parseSum p
      | otherwise = fmap (fmap M1) . consParseJSON (cname :* p)
    where
      cname = conName (undefined :: M1 _i c _f _p)

-- multiple constructors
instance (AllNullary (a :+: b) allNullary, ParseSum (a :+: b) allNullary)
  => GFromJSONWithWarnings' (a :+: b) where
  -- If all constructors of a sum datatype are nullary and the
  -- 'allNullaryToStringTag' option is set they are expected to be
  -- encoded as strings.  This distinction is made by 'parseSum':
  gParseJSONWithWarnings' p = (unTagged :: Tagged allNullary
    (Parser (WithWarnings ((a :+: b) _d))) -> Parser (WithWarnings ((a :+: b) _d))) .
      parseSum p

-- Parse sum
class ParseSum f allNullary where
  parseSum :: TypeName :* Options -> Value
    -> Tagged allNullary (Parser (WithWarnings (f a)))

-- Parse sum where not all constructors are nullary (i.e. JSON object)
instance (ConstructorNames f, FromPair f, FromTaggedObject f,
  FromUntaggedValue f) => ParseSum f False where
    parseSum p = Tagged . parseNonAllNullarySum p

instance (ConstructorNames f, SumFromString f, FromPair f, FromTaggedObject f,
  FromUntaggedValue f) => ParseSum f True where
  parseSum p@(tname :* opts)
    | allNullaryToStringTag opts = Tagged . fmap pure . parseAllNullarySum tname opts
    | otherwise = Tagged . parseNonAllNullarySum p

parseAllNullarySum :: (SumFromString f, ConstructorNames f)
  => TypeName -> Options -> Value -> Parser (f a)
parseAllNullarySum tname opts = withText tname $
  \tag -> maybe (badTag tag) return $ parseSumFromString modifier tag
  where
    badTag tag = failWithCTags tname modifier $ \cnames ->
      "expected one of the tags " ++ show cnames ++
      ", but found tag " ++ show tag
    modifier = constructorTagModifier opts

failWithCTags :: forall f a t. ConstructorNames f
  => TypeName -> (String -> t) -> ([t] -> String) -> Parser (f a)
failWithCTags tname modifier f = contextType tname . fail $ f cnames
  where
    cnames = (constructorTags modifier :: Tagged2 f [t]).unTagged2

parseNonAllNullarySum :: forall f c .  (FromPair f, FromTaggedObject f,
  FromUntaggedValue f, ConstructorNames f)
  => TypeName :* Options -> Value -> Parser (WithWarnings (f c))
parseNonAllNullarySum = undefined

-- for enums
class SumFromString f where
  parseSumFromString :: (String -> String) -> Text -> Maybe (f a)

instance (SumFromString a, SumFromString b) => SumFromString (a :+: b) where
  parseSumFromString opts key = (L1 <$> parseSumFromString opts key) <|>
    (R1 <$> parseSumFromString opts key)

instance Constructor c => SumFromString (C1 c U1) where
  parseSumFromString modifier key
      | key == name = Just $ M1 U1
      | otherwise = Nothing
    where
      name = pack $ modifier $ conName (undefined :: M1 _i c _f _p)

-- parse JSON (key, value)
class FromPair f where
  -- The first component of the parameter tuple is the tag to match.
  parsePair :: Key :* TypeName :* Options -> Value ->
    Maybe (Parser (WithWarnings (f a)))

-- Sum from pair
instance (FromPair a, FromPair b) => FromPair (a :+: b) where
  parsePair p pair = fmap (fmap L1) <$> parsePair p pair <|>
    fmap (fmap R1) <$> parsePair p pair

instance (Constructor c, ConsFromJSON a) => FromPair (C1 c a) where
  parsePair (tag :* p@(_ :* opts)) v
      | tag == tag' = Just $ fmap M1 <$> consParseJSON (cname :* p) v
      | otherwise = Nothing
    where
      tag' = Key.fromString $ constructorTagModifier opts cname
      cname = conName (undefined :: M1 _i c _a _p)

-- from JSON Object
class FromTaggedObject f where
  parseFromTaggedObject :: Text :* String :* TypeName :* Options -> Object ->
    Maybe (Parser (WithWarnings (f a)))

instance ( FromTaggedObject a, FromTaggedObject b) =>
  FromTaggedObject (a :+: b) where
  parseFromTaggedObject p obj =
    (fmap (fmap L1) <$> parseFromTaggedObject p obj) <|>
    (fmap (fmap R1) <$> parseFromTaggedObject p obj)

instance (IsRecord f isRecord, FromTaggedObject' f isRecord, Constructor c)
  => FromTaggedObject (C1 c f) where
  parseFromTaggedObject (tag :* contentsFieldName :* p@(_ :* opts))
      | tag == tag' = Just . fmap (fmap M1) .
        (unTagged :: Tagged isRecord (Parser (WithWarnings (f a))) ->
          Parser (WithWarnings (f a))) . parseFromTaggedObject'
            (contentsFieldName :* cname :* p)
      | otherwise = const Nothing
    where
      tag' = pack $ constructorTagModifier opts cname
      cname = conName (undefined :: M1 _i c _f _p)

class FromTaggedObject' f isRecord where
  parseFromTaggedObject' :: String :* ConName :* TypeName :* Options -> Object ->
    Tagged isRecord (Parser (WithWarnings (f a)))

instance (RecordFromJSON f, FieldNames f) => FromTaggedObject' f True where
  -- Records are unpacked in the tagged object
  parseFromTaggedObject' (_ :* p) = Tagged . recordParseJSON (True :* p)

instance (ConsFromJSON f) => FromTaggedObject' f False where
  -- Nonnullary nonrecords are encoded in the contents field
  parseFromTaggedObject' p obj = Tagged $ do
      contents <- contextCons cname tname (obj .: key)
      consParseJSON p' contents <?> Key key
    where
      key = Key.fromString contentsFieldName
      contentsFieldName :* p'@(cname :* tname :* _) = p

instance {-# OVERLAPPING #-} FromTaggedObject' U1 False where
  -- Nullary constructors don't need a contents field
  parseFromTaggedObject' _ _ = Tagged (pure $ pure U1)

-- finally parse record
class RecordFromJSON f where
  recordParseJSON :: Bool :* ConName :* TypeName :* Options -> Object ->
    Parser (WithWarnings (f a))

instance (FieldNames f, RecordFromJSON' f) => RecordFromJSON f where
  recordParseJSON (fromTaggedSum :* p@(cname :* tname :* opts)) = \obj ->
    checkUnknown obj >> recordParseJSON' p obj
      where
        knownFields :: KM.KeyMap ()
        knownFields = KM.fromList $ P.map ((,()) . Key.fromString) $
            [tagFieldName (sumEncoding opts) | fromTaggedSum] <>
            (fieldLabelModifier opts <$> fieldNames (undefined :: f a) [])

        checkUnknown =
            if not (rejectUnknownFields opts)
            then \_ -> return $ pure ()
            else \obj -> case KM.keys (KM.difference obj knownFields) of
              [] -> return $ pure @WithWarnings ()
              unknownFields -> contextCons cname tname $
                  fail ("unknown fields: " ++ show unknownFields)

class RecordFromJSON' f where
  recordParseJSON' :: ConName :* TypeName :* Options -> Object ->
    Parser (WithWarnings (f a))

-- accumulate warnings for product
instance (RecordFromJSON' a, RecordFromJSON' b) => RecordFromJSON' (a :*: b) where
  recordParseJSON' p obj = liftA2 (:*:) <$> recordParseJSON' p obj <*>
    recordParseJSON' p obj

-- metadata
instance {-# OVERLAPPABLE #-} RecordFromJSON' f => RecordFromJSON' (M1 i s f) where
  recordParseJSON' args obj = fmap M1 <$> recordParseJSON' args obj

-- field with value
instance (Selector s, FromJSONWithWarnings a, Generic a, K1 i a ~ Rep a)
  => RecordFromJSON' (S1 s (K1 i a)) where
  recordParseJSON' args obj = recordParseJSONImpl
    gParseJSONWithWarnings args obj

-- recursive field
instance {-# OVERLAPPING #-} (Selector s, FromJSONWithWarnings a)
  => RecordFromJSON' (S1 s (Rec0 a)) where
  recordParseJSON' args obj = recordParseJSONImpl gParseJSONWithWarnings args obj

recordParseJSONImpl :: forall s a f i . Selector s
  => (Options -> Value -> Parser (WithWarnings (f a)))
  -> (ConName :* TypeName :* Options) -> Object -> Parser (WithWarnings (M1 i s f a))
recordParseJSONImpl parseVal (cname :* tname :* opts) obj = do
    fv <- contextCons cname tname (obj .: label)
    fmap M1 <$> parseVal opts fv <?> Key label
  where
    label = Key.fromString $ fieldLabelModifier opts sname
    sname = selName (undefined :: M1 _i s _f _p)

-- from JSON value
class FromUntaggedValue f where
  parseUntaggedValue :: TypeName :* Options -> Value -> Parser (WithWarnings (f a))

-- sum
instance (FromUntaggedValue a, FromUntaggedValue b)
  => FromUntaggedValue (a :+: b) where
  parseUntaggedValue p value = fmap L1 <$> parseUntaggedValue p value <|>
    fmap R1 <$> parseUntaggedValue p value

instance {-# OVERLAPPABLE #-} (ConsFromJSON a, Constructor c)
  => FromUntaggedValue (C1 c a) where
  parseUntaggedValue p = fmap (fmap M1) . consParseJSON (cname :* p)
    where
      cname = conName (undefined :: M1 _i c _f _p)

instance {-# OVERLAPPING #-} Constructor c => FromUntaggedValue (C1 c U1) where
  parseUntaggedValue (tname :* opts) v = fmap pure $ contextCons cname tname $
    case v of
      String tag
          | tag == tag' -> pure $ M1 U1
          | otherwise -> fail_ tag
      _ -> typeMismatch "String" v
    where
      tag' = pack $ constructorTagModifier opts cname
      cname = conName (undefined :: M1 _i c _f _p)
      fail_ tag = fail $
        "expected tag " ++ show tag' ++ ", but found tag " ++ show tag

-- constructors
-- | Constructors need to be decoded differently depending on whether they're
-- a record or not. This distinction is made by 'ConsParseJSON'.
class ConsFromJSON f where
  consParseJSON :: ConName :* TypeName :* Options -> Value ->
    Parser (WithWarnings (f a))

class ConsFromJSON' f isRecord where
  consParseJSON' :: ConName :* TypeName :* Options -> Value ->
    Tagged isRecord (Parser (WithWarnings (f a)))

instance (IsRecord f isRecord, ConsFromJSON' f isRecord) => ConsFromJSON f where
  consParseJSON p = (unTagged :: Tagged isRecord (Parser (WithWarnings (f a)))->
    Parser (WithWarnings (f a))) . consParseJSON' p

instance {-# OVERLAPPING #-} (GFromJSONWithWarnings a, RecordFromJSON (S1 s a))
  => ConsFromJSON' (S1 s a) True where
  consParseJSON' p@(cname :* tname :* opts)
    | unwrapUnaryRecords opts = Tagged . fmap (fmap M1) .
      gParseJSONWithWarnings opts
    | otherwise = Tagged . withObject (showCons cname tname) (recordParseJSON (False :* p))

instance RecordFromJSON f => ConsFromJSON' f True where
  consParseJSON' p@(cname :* tname :* _) = Tagged . withObject
    (showCons cname tname) (recordParseJSON (False :* p))

instance {-# OVERLAPPING #-} ConsFromJSON' U1 False where
  consParseJSON' (cname :* tname :* _) v =
    Tagged . contextCons cname tname $ case v of
        Array a
          | V.null a -> pure $ pure U1
          | otherwise -> fail_ a
        _ -> typeMismatch "Array" v
    where
      fail_ a = fail $
          "expected an empty Array, but encountered an Array of length " ++
          show (V.length a)

instance {-# OVERLAPPING #-} GFromJSONWithWarnings f => ConsFromJSON' (S1 s f) False where
  consParseJSON' (_ :* _ :* opts) = Tagged . fmap (fmap M1) . gParseJSONWithWarnings opts

instance (ProductFromJSON f, ProductSize f) => ConsFromJSON' f False where
  consParseJSON' p = Tagged . productParseJSON0 p

class ProductFromJSON f where
  productParseJSON :: ConName :* TypeName :* Options -> Array -> Int ->
    Int -> Parser (WithWarnings (f a))

instance (ProductFromJSON a, ProductFromJSON b) => ProductFromJSON (a :*: b) where
  productParseJSON p arr ix len = liftA2 (:*:) <$>
    productParseJSON p arr ix lenL <*> productParseJSON p arr ixR lenR
    where
      lenL = len `unsafeShiftR` 1
      ixR  = ix + lenL
      lenR = len - lenL

instance GFromJSONWithWarnings a => ProductFromJSON (S1 s a) where
  productParseJSON (_ :* _ :* opts) arr ix _ = fmap M1 <$>
    gParseJSONWithWarnings opts (V.unsafeIndex arr ix) <?> Index ix

productParseJSON0 :: forall f a. (ProductFromJSON f, ProductSize f)
  => ConName :* TypeName :* Options -> Value -> Parser (WithWarnings (f a))
  -- Products are expected to be encoded to an array. Here we check whether we
  -- got an array of the same size as the product, then parse each of the
  -- product's elements using productParseJSON:
productParseJSON0 p@(cname :* tname :* _) =
    withArray (showCons cname tname) $ \arr ->
        let lenArray = V.length arr
            lenProduct = (productSize :: Tagged2 f Int).unTagged2 in
        if lenArray == lenProduct
        then productParseJSON p arr 0 lenProduct
        else contextCons cname tname $
             fail $ "expected an Array of length " ++ show lenProduct ++
                    ", but encountered an Array of length " ++ show lenArray


