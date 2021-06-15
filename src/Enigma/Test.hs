{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Enigma.Test where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator.Wallet

import           Enigma.OnChain
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Plutus.Contract      as Contract hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import qualified Ledger.Value             as Value
import           Playground.Contract  (ToSchema)
import           Prelude              (Semigroup (..))
import qualified Prelude
import qualified Ledger.Contexts          as Validation
import qualified Ledger.Contexts                  as V
import           Enigma.Seals  as Seals
import qualified Data.Text as T
import qualified Data.ByteString.Char8        as C
import           Playground.Contract

import           Ledger.Scripts            (Redeemer(..))

import           Ledger                   (CurrencySymbol, MonetaryPolicyHash, txId, PubKeyHash, Tx, TxOutTx (..), ValidatorHash, Address(..), ScriptContext, scriptAddress, mkMonetaryPolicyScript)
import           Plutus.V1.Ledger.Credential (Credential (..), StakingCredential)
import           Playground.Contract
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..))
import qualified Ledger.Contexts          as Validation
import           Plutus.Contract        as Contract hiding (when)
import qualified PlutusTx             as PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup (..), unless, Applicative (..), String, foldMap)
import qualified Ledger.Contexts                  as V
import qualified Data.Map                         as Map
import qualified Ledger.Ada                       as Ada
import qualified Data.Text as T
import Text.Printf
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Plutus.Trace.Emulator  as Emulator
import           Data.Void              (Void)
import           Prelude                  (Semigroup (..), foldMap)
import           Enigma.Seals  as Seals
import qualified Ledger.Tx                as Tx
import qualified Data.ByteString.Base16 as BS16 (decode, encode)
import qualified Data.ByteString        as BS
import           Data.Word              (Word8)
import qualified Ledger.Typed.Scripts     as Scripts hiding (validatorHash)

import           Plutus.V1.Ledger.Address              (Address (..), scriptHashAddress)

assetSymbol :: CurrencySymbol
assetSymbol = "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e"

assetToken :: TokenName
assetToken = "C"

test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1 .. 10]]

    v :: Value
    v = Ada.lovelaceValueOf                    100_000_000 <>
        Value.singleton assetSymbol assetToken 100_000_000

myTrace :: EmulatorTrace ()
myTrace = do

    h1 <- activateContractWallet (Wallet 1)  endpoints

    callEndpoint @"create" h1 $ CreateParams
        { cubeIdCurrency  = "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e" 
        , cubeIdTokenName = "B"
        , stateMachineNftCurrency  = "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e"
        , stateMachineNftTokenName = "C"
        , firstRewardCurrency  = "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e"
        , firstRewardTokenName = "A"
        , secondRewardCurrency  = "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e"
        , secondRewardTokenName = "D"
        , thirdRewardCurrency  = "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e"
        , thirdRewardTokenName = "F"
        , lastReardCurrency  = "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e"
        , lastReardTokenName = "X" 
        , fiAnswer     =  sha2_256(C.pack ("A" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )  
        , seAnswer    = sha2_256 (C.pack ("B"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") ) 
        , thAnswer     = sha2_256 (C.pack ("C"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") ) 
        , foAnswer    = sha2_256 (C.pack ("D"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e")  )
        , fvAnswer     = sha2_256 (C.pack ("E"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e")  )
        , siAnswer    = sha2_256 (C.pack ("F"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e")  )
        , svAnswer  = sha2_256 (C.pack ("G"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e")  )
        , eiAnswer     = sha2_256 (C.pack ("H"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e")  )
        , niAnswer    =  sha2_256(C.pack ("I"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e")  )
        , teAnswer    = sha2_256 (C.pack ("J"  ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e")  )
        }
    void $ Emulator.waitNSlots 10

    callEndpoint @"solve" h1 $ SolveParams
        { sCubeId          = AssetClass ("c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e", "B")
        , sStateMachineNft = AssetClass ("c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e", "C")
        , sFirstReward     = AssetClass ("c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e", "A")
        , sSecondReward    = AssetClass ("c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e", "D")
        , sThirdReward     = AssetClass ("c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e", "F")
        , sLastReard       = AssetClass ("c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e", "X")
        , sFirstAnswer     = HashedString $ sha2_256 $ sha2_256(C.pack ("A" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )  
        , sSecondAnswer    = HashedString $ sha2_256 $ sha2_256(C.pack ("B" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sThirdAnswer     = HashedString $ sha2_256 $ sha2_256(C.pack ("C" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sFourthAnswer    = HashedString $ sha2_256 $ sha2_256(C.pack ("D" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sFifthAnswer     = HashedString $ sha2_256 $ sha2_256(C.pack ("E" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sSixthAnswer     = HashedString $ sha2_256 $ sha2_256(C.pack ("F" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sSeventhAnswer   = HashedString $ sha2_256 $ sha2_256(C.pack ("G" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sEightAnswer     = HashedString $ sha2_256 $ sha2_256(C.pack ("H" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sNinethAnswer    = HashedString $ sha2_256 $ sha2_256(C.pack ("I" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sTenthAnswer     = HashedString $ sha2_256 $ sha2_256(C.pack ("J" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
        , sPuzzleIndex     = 0
        , sAnswer          = sha2_256(C.pack ("A" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
    }
    void $ Emulator.waitNSlots 1 