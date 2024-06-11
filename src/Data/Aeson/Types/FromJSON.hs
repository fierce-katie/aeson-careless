-- Based on https://hackage.haskell.org/package/aeson-2.2.2.0/docs/src/Data.Aeson.Types.FromJSON.html
{-# LANGUAGE PartialTypeSignatures #-}
module Data.Aeson.Types.FromJSON where

import Data.Aeson.Types
import Data.Aeson.Types.Generic
import qualified Data.Aeson.Key as Key
import qualified Data.DList as DList
import GHC.Generics

-- Information for error messages

type TypeName = String
type ConName = String

-- | Add the name of the type being parsed to a parser's error messages.
contextType :: TypeName -> Parser a -> Parser a
contextType = prependContext

-- | Add the tagKey that will be looked up while building an ADT
-- | Produce the error equivalent to
-- | Left "Error in $: parsing T failed, expected an object with keys "tag" and
-- | "contents", where "tag" i-- |s associated to one of ["Foo", "Bar"],
-- | The parser returned error was: could not find key "tag"
contextTag :: Key -> [String] -> Parser a -> Parser a
contextTag tagKey cnames = prependFailure
  ("expected Object with key \"" ++ Key.toString tagKey ++ "\"" ++
  " containing one of " ++ show cnames ++ ", ")

-- | Add the name of the constructor being parsed to a parser's error messages.
contextCons :: ConName -> TypeName -> Parser a -> Parser a
contextCons cname tname = prependContext (showCons cname tname)

-- | Render a constructor as @\"MyType(MyConstructor)\"@.
showCons :: ConName -> TypeName -> String
showCons cname tname = tname ++ "(" ++ cname ++ ")"

-- | Add context to a failure message, indicating the name of the structure
-- being parsed.
--
-- > prependContext "MyType" (fail "[error message]")
-- > -- Error: "parsing MyType failed, [error message]"
prependContext :: String -> Parser a -> Parser a
prependContext name = prependFailure ("parsing " ++ name ++ " failed, ")

-- | List of all constructor tags.
constructorTags :: ConstructorNames a => (String -> t) -> Tagged2 a [t]
constructorTags modifier =
    fmap DList.toList (constructorNames' modifier)

-- | List of all constructor names of an ADT, after a given conversion
-- function. (Better inlining.)
class ConstructorNames a where
    constructorNames' :: (String -> t) -> Tagged2 a (DList.DList t)

instance (ConstructorNames a, ConstructorNames b) => ConstructorNames (a :+: b) where
    constructorNames' = liftA2 append constructorNames' constructorNames'
      where
        append
          :: Tagged2 a (DList.DList t)
          -> Tagged2 b (DList.DList t)
          -> Tagged2 (a :+: b) (DList.DList t)
        append (Tagged2 xs) (Tagged2 ys) = Tagged2 (DList.append xs ys)
    {-# INLINE constructorNames' #-}

instance Constructor c => ConstructorNames (C1 c a) where
    constructorNames' f = Tagged2 (pure (f cname))
      where
        cname = conName (undefined :: M1 _i c _f _p)
    {-# INLINE constructorNames' #-}

class FieldNames f where
    fieldNames :: f a -> [String] -> [String]

instance (FieldNames a, FieldNames b) => FieldNames (a :*: b) where
    fieldNames _ =
      fieldNames (undefined :: a x) .
      fieldNames (undefined :: b y)
    {-# INLINE fieldNames #-}

instance (Selector s) => FieldNames (S1 s f) where
    fieldNames _ = (selName (undefined :: M1 _i s _f _p) :)
    {-# INLINE fieldNames #-}
