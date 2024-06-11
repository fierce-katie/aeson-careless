-- Based on https://hackage.haskell.org/package/aeson-2.2.2.0/docs/src/Data.Aeson.Types.Generic.html
{-
Copyright (c) 2011, MailRank, Inc. 2014-2021 Aeson project contributors

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}
module Data.Aeson.Types.Generic
    (
      IsRecord
    , AllNullary
    , Tagged2(..)
    , True
    , False
    , And
    , Zero
    , One
    , ProductSize(..)
    , (:*)(..)
    ) where

import Data.Kind (Type)

import GHC.Generics

--------------------------------------------------------------------------------

class IsRecord (f :: Type -> Type) isRecord | f -> isRecord

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
instance {-# OVERLAPPING #-} IsRecord (M1 S ('MetaSel 'Nothing u ss ds) f) False
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord Par1 True
instance IsRecord (Rec1 f) True
instance IsRecord (f :.: g) True
instance IsRecord U1 False

--------------------------------------------------------------------------------

class AllNullary (f :: Type -> Type) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) False
instance AllNullary (a :.: b) False
instance AllNullary (K1 i c) False
instance AllNullary Par1 False
instance AllNullary (Rec1 f) False
instance AllNullary U1 True

newtype Tagged2 (s :: Type -> Type) b = Tagged2 {unTagged2 :: b}
  deriving Functor

--------------------------------------------------------------------------------

data True
data False

class    And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True  True  True
instance And False False False
instance And False True  False
instance And True  False False

--------------------------------------------------------------------------------

-- | A type-level indicator that 'ToJSON' or 'FromJSON' is being derived generically.
data Zero

-- | A type-level indicator that 'ToJSON1' or 'FromJSON1' is being derived generically.
data One

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ (productSize :: Tagged2 a Int).unTagged2 +
                            (productSize :: Tagged2 b Int).unTagged2

instance ProductSize (S1 s a) where
    productSize = Tagged2 1

--------------------------------------------------------------------------------

-- | Simple extensible tuple type to simplify passing around many parameters.
data a :* b = a :* b

infixr 1 :*
