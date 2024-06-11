module Data.Aeson.Careless.Types where

import Control.Monad.Writer
import Data.List.NonEmpty as NE
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

