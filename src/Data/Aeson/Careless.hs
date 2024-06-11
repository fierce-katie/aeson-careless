module Data.Aeson.Careless where

import Control.Lens
import Control.Monad.Writer
import Data.Aeson
import Data.Aeson.Types
import Data.Either
import Data.List.NonEmpty as NE
import Data.Foldable as F
import Data.Text as T
import GHC.IsList as IL
import GHC.Generics

-- Types

type WithWarnings = Writer [Text]

mWarn :: MonadFail m => Text -> WithWarnings (m a)
mWarn = writer . (fail "Couldn't parse",) . pure

emptyResp :: MonadFail m => WithWarnings (m a)
emptyResp = pure (fail "Couldn't parse")

-- A list that can be partially parsed from JSON, wrong values are skipped,
-- warnings accumulated.
newtype ListW a = ListW { getListW :: [a] }
  deriving newtype (Eq, Show, Generic, IsList, Semigroup, Monoid,
    Functor, Foldable, Applicative, Monad, MonadFail)

lfilter :: (IsList (l a), Item (l a) ~ a) => (a -> Bool) -> l a -> l a
lfilter f = IL.fromList . Prelude.filter f . IL.toList

-- An optional value that results in Nothing instead of failure when JSON
-- parsing fails, producing a warning.
newtype MaybeW a = MaybeW { getMaybeW :: Maybe a }
  deriving newtype (Eq, Show, Generic, Functor, Foldable, Applicative, Monad, MonadFail)

justW :: a -> MaybeW a
justW = MaybeW . Just

nothingW :: MaybeW a
nothingW = MaybeW Nothing

-- Non-empty list that can be prtially parsed from JSON, but fails when there
-- are no correct values. Warnings are accumulated.
newtype NonEmptyW a = NonEmptyW { getNonEmptyW :: NonEmpty a }
  deriving newtype (Eq, Show, Generic, Functor, Foldable, IsList)

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
          Prelude.zip [0..] vs
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
