-- 
--  Copyright (c) 2021 Angel Castillo.
-- 
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
-- 
--    http://www.apache.org/licenses/LICENSE-2.0
-- 
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
--

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Enigma.Definitions.Seals(
      Seals (..)
    , getSeals
    , sealsSymbol
    , sealsToken
    -- * Constructors
    , fromValue
    , toValue
    , sealsOf
    , sealsValueOf
    -- * Num operations
    , divide
    -- * Etc.
    , isZero
    , policy
    , valueOf'
    , fromValue'
    , substract
    ) where

import qualified Prelude                          as Haskell

import           Data.Fixed

import           Codec.Serialise.Class            (Serialise)
import           Data.Aeson                       (FromJSON, ToJSON)
-- | import           Data.Tagged
import           Data.Text.Prettyprint.Doc.Extras
import           GHC.Generics                     (Generic)
import qualified Plutus.V1.Ledger.Value           as TH
import qualified PlutusTx                         as PlutusTx
import           PlutusTx.Lift                    (makeLift)
import           PlutusTx.Prelude                 hiding (divide)
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger                           (txId, PubKeyHash, Tx, TxOutTx (..), ValidatorHash, Address(..), ScriptContext, scriptAddress, mkMonetaryPolicyScript)
import qualified Ledger.Contexts                  as V
import qualified PlutusTx.Prelude                 as P
import qualified Data.ByteString.Char8 as C 
import qualified Ledger.Tx                as Tx

-- | Mint Policies 

mkPolicy :: ScriptContext -> Bool
mkPolicy _ = True

policy :: Scripts.MonetaryPolicy
policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||])

{-# INLINABLE sealsSymbol #-}
-- | The 'CurrencySymbol' of the 'Seals' currency.
sealsSymbol :: TH.CurrencySymbol
sealsSymbol = V.scriptCurrencySymbol policy

{-# INLINABLE sealsToken #-}
-- | The 'TokenName' of the 'Seals' currency.
sealsToken :: TH.TokenName
sealsToken = TH.TokenName "Seal"

-- | The special currency on the Cardano blockchain. The unit of Seals is Seal
newtype Seals = Seals { getSeals :: Integer }
    deriving (Enum)
    deriving stock (Haskell.Eq, Haskell.Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
    deriving newtype (Eq, Ord, Haskell.Num, AdditiveSemigroup, AdditiveMonoid, AdditiveGroup, MultiplicativeSemigroup, MultiplicativeMonoid, Integral, Real, Serialise, PlutusTx.IsData)

instance Haskell.Semigroup Seals where
    Seals a1 <> Seals a2 = Seals (a1 + a2)

instance Semigroup Seals where
    Seals a1 <> Seals a2 = Seals (a1 + a2)

instance Haskell.Monoid Seals where
    mempty = Seals 0

instance Monoid Seals where
    mempty = Seals 0

makeLift ''Seals

{-# INLINABLE toValue #-}
-- | Create a 'Value' containing only the given 'Seals'.
toValue :: Seals -> TH.Value
toValue (Seals i) = TH.singleton sealsSymbol sealsToken i

{-# INLINABLE fromValue #-}
-- | Get the 'Seals' in the given 'Value'.
fromValue :: TH.Value -> Seals
fromValue v = Seals (TH.valueOf v sealsSymbol sealsToken)

{-# INLINABLE sealsOf #-}
-- | Create 'Seals' representing the given quantity of Seals (the unit of the currency Seals).
sealsOf :: Integer -> Seals
sealsOf = Seals

{-# INLINABLE sealsValueOf #-}
-- | A 'Value' with the given amount of Seals (the currency unit).
--
--   @sealsValueOf == toValue . sealsOf@
--
sealsValueOf :: Integer -> TH.Value
sealsValueOf = TH.singleton sealsSymbol sealsToken

{-# INLINABLE divide #-}
-- | Divide one 'Seals' value by another.
divide :: Seals -> Seals -> Seals
divide (Seals a) (Seals b) = Seals (P.divide a b)

{-# INLINABLE substract #-}
-- | Substract one 'substract' value by another.
substract :: Seals -> Seals -> Seals
substract (Seals a) (Seals b) = Seals (a - b)

{-# INLINABLE isZero #-}
-- | Check whether an 'Seals' value is zero.
isZero :: Seals -> Bool
isZero (Seals i) = i == 0

{-# INLINABLE fromValue' #-}
-- | Get the 'Seals' in the given 'Value'.
fromValue' :: TH.CurrencySymbol-> TH.TokenName -> TH.Value -> Seals
fromValue' cs tn v = Seals (TH.valueOf v cs tn)

{-# INLINABLE valueOf' #-}
-- | A 'Value' with the given amount of Seals (the currency unit).
--
--   @sealsValueOf == toValue . sealsOf@
--
valueOf' :: TH.CurrencySymbol-> TH.TokenName -> Integer -> TH.Value
valueOf' cs tn = TH.singleton cs tn