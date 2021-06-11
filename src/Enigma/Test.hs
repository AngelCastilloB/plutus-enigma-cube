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

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Enigma.Test where

-- This is a starter contract, based on the Game contract,
-- containing the bare minimum required scaffolding.
--
-- What you should change to something more suitable for
-- your use case:
--   * The MyDatum type
--   * The MyMyRedeemerValue type
--
-- And add function implementations (and rename them to
-- something suitable) for the endpoints:
--   * publish
--   * redeem

import           Control.Monad        (void)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value             (Value)
import           Ledger.Scripts            (Redeemer(..))
import qualified Ledger.Value             as Value
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
import PlutusTx.Builtins
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
import           Enigma.Definitions.Seals  as Seals
import qualified Ledger.Tx                as Tx
import qualified Data.ByteString.Base16 as BS16 (decode, encode)
import qualified Data.ByteString        as BS
import           Data.Word              (Word8)
-- | Mint Policies 

-- | mkPolicy :: ScriptContext -> Bool
-- | mkPolicy _ = True

-- | policy :: Scripts.MonetaryPolicy
-- | policy = mkMonetaryPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy mkPolicy ||])

-- | curSymbol :: Value.CurrencySymbol
-- | curSymbol = V.scriptCurrencySymbol policy

-- | These are the data script and redeemer types. We are using an integer
--   value for both, but you should define your own types.

data ContractParam = ContractParam
    { monetaryPolicyHash :: CurrencySymbol
    , tokenName    :: TokenName
    , answer1      :: Integer
    , answer2      :: Integer
    , answer3      :: Integer
    , answer4      :: Integer
    , answer5      :: Integer
    , answer6      :: Integer
    , answer7      :: Integer
    , answer8      :: Integer
    , answer9      :: Integer
    , answer10     :: Integer
    , answer11     :: Integer
    , answer12     :: Integer
    } deriving Show

PlutusTx.unstableMakeIsData ''ContractParam
PlutusTx.makeLift ''ContractParam

newtype MyDatum = MyDatum Integer deriving newtype PlutusTx.IsData
PlutusTx.makeLift ''MyDatum

newtype MyRedeemer = MyRedeemer Integer deriving newtype PlutusTx.IsData
PlutusTx.makeLift ''MyRedeemer

validatorHashOutputsAt :: ValidatorHash -> TxInfo -> [Value]
validatorHashOutputsAt pk p =
    let flt V.TxOut{V.txOutAddress = Address (ScriptCredential pk') _, V.txOutValue} | pk == pk' = Just txOutValue
        flt _                             = Nothing
    in mapMaybe flt (txInfoOutputs p)

-- | Get the total value paid to a public key address by a pending transaction.
valuePaidToScript :: TxInfo -> ValidatorHash -> Value
valuePaidToScript ptx pkh = mconcat (validatorHashOutputsAt pkh ptx)

checkAnswer:: Integer -> ContractParam -> Integer -> Bool;
checkAnswer seals param redeemer
					| seals == 12 = (answer1 param) == redeemer
                    | seals == 11 = (answer2 param) == redeemer
                    | seals == 10 = (answer3 param) == redeemer
                    | seals == 9 = (answer4 param) == redeemer
                    | seals == 8 = (answer5 param) == redeemer
                    | seals == 7 = (answer6 param) == redeemer
                    | seals == 6 = (answer7 param) == redeemer
                    | seals == 5 = (answer8 param) == redeemer
                    | seals == 4 = (answer9 param) == redeemer
                    | seals == 3 = (answer10 param) == redeemer
                    | seals == 2 = (answer11 param) == redeemer
                    | seals == 1 = (answer12 param) == redeemer
					| otherwise = False

-- | This method is the spending validator (which gets lifted to
--   its on-chain representation).
validateSpend :: ContractParam -> MyDatum -> MyRedeemer -> ScriptContext -> Bool
validateSpend param (MyDatum num) (MyRedeemer num2) ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} =
    let
        forged            = V.txInfoForge txInfo
        expected          = Seals.valueOf' (monetaryPolicyHash param) (tokenName param) (-1)
        newValueLocked    = getSeals $ Seals.fromValue' (monetaryPolicyHash param) (tokenName param) (Validation.valueLockedBy txInfo (Validation.ownHash ctx))
        valueSpent        = getSeals $ Seals.fromValue' (monetaryPolicyHash param) (tokenName param) (Validation.valueSpent txInfo)
        valPaidToScript   = getSeals $ Seals.fromValue' (monetaryPolicyHash param) (tokenName param) (valuePaidToScript txInfo (Validation.ownHash ctx))
    in traceIfFalse "Value burned different than 1" (valPaidToScript == (valueSpent - 1)) && 
       traceIfFalse "Value forged different from expected" (expected == forged) &&
       traceIfFalse "Incorrect answer" (checkAnswer valueSpent param num2)


-- | The address of the contract (the hash of its validator script).
contractAddress :: ContractParam -> Address
contractAddress = Ledger.scriptAddress . (Scripts.validatorScript . starterInstance)

data Starter
instance Scripts.ScriptType Starter where
    type instance RedeemerType Starter = MyRedeemer
    type instance DatumType Starter = MyDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
starterInstance :: ContractParam -> Scripts.ScriptInstance Starter
starterInstance param = Scripts.validator @Starter
    ($$(PlutusTx.compile [|| validateSpend ||]) `PlutusTx.applyCode` PlutusTx.liftCode param)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @MyDatum @MyRedeemer

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)


-- | The schema of the contract, with two endpoints.
type Schema =
    BlockchainActions
        .\/ Endpoint "publish" (Integer, Value)
        .\/ Endpoint "redeem" Integer
        .\/ Endpoint "mint" MintParams

contract :: Contract () Schema T.Text ()
contract = (publish `select` redeem) `select` mint

scriptParams :: ContractParam 
scriptParams = ContractParam
                    { monetaryPolicyHash = Seals.sealsSymbol
                    , tokenName    = Seals.sealsToken
                    , answer1      = 1 
                    , answer2      = 2 
                    , answer3      = 3 
                    , answer4      = 4 
                    , answer5      = 5 
                    , answer6      = 6 
                    , answer7      = 7 
                    , answer8      = 8 
                    , answer9      = 9 
                    , answer10     = 10 
                    , answer11     = 11 
                    , answer12     = 12 
                    }
mint :: Contract () Schema T.Text ()
mint = do
    MintParams name amount <- endpoint @"mint"
    let val     = Value.singleton Seals.sealsSymbol Seals.sealsToken amount
        datum   = MyDatum 100
        lookups =  Constraints.scriptInstanceLookups (starterInstance scriptParams) <> Constraints.monetaryPolicy Seals.policy
        tx      = (Constraints.mustForgeValue val) <> (Constraints.mustPayToTheScript datum val)  
    ledgerTx <- submitTxConstraintsWith @Starter lookups tx

    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo $ show val

-- | The "publish" contract endpoint.
publish :: Contract () Schema T.Text ()
publish = do
    (i, lockedFunds) <- endpoint @"publish"
    let scriptParams = ContractParam
                        { monetaryPolicyHash = Seals.sealsSymbol
                        , tokenName    = Seals.sealsToken
                        }
        tx = Constraints.mustPayToTheScript (MyDatum i) lockedFunds
    void $ submitTxConstraints (starterInstance scriptParams) tx

-- | The "redeem" contract endpoint.
redeem :: Contract () Schema T.Text ()
redeem = do
    i <- endpoint @"redeem"
    utxos <- utxoAt $ contractAddress scriptParams
    let orefs   = fst <$> Map.toList utxos
    Contract.logInfo $ show orefs
    let sealsInOutputs = Seals.fromValue (foldMap (Tx.txOutValue . Tx.txOutTxOut) utxos)
    let burnAmount     = Seals.sealsOf 1
    let val            = Seals.toValue $ (Seals.substract sealsInOutputs $ burnAmount)
    Contract.logInfo $ show sealsInOutputs
    Contract.logInfo $ show burnAmount
    Contract.logInfo $ show val

    let datum   = MyDatum i
        lookups =  Constraints.unspentOutputs utxos <> Constraints.otherScript ((Scripts.validatorScript . starterInstance) scriptParams) <> Constraints.scriptInstanceLookups (starterInstance scriptParams) <> Constraints.monetaryPolicy Seals.policy
        tx      = mconcat [Constraints.mustSpendScriptOutput oref (Redeemer  (PlutusTx.toData (MyRedeemer i))) | oref <- orefs]
           <> (Constraints.mustForgeValue (Seals.sealsValueOf (-1)))
           <> (Constraints.mustPayToTheScript datum val)  

    ledgerTx <- submitTxConstraintsWith @Starter lookups tx

    void $ awaitTxConfirmed $ txId ledgerTx

    
endpoints :: Contract () Schema T.Text ()
endpoints = contract


mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = "name"
        , mpAmount    = 12
        }
    void $ Emulator.waitNSlots 1 

    callEndpoint @"redeem" h2 $ 1

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 2

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 3

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 4

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 5

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 6

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 7

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 8

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 9

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 10

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 11

    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"redeem" h3 $ 12

    void $ Emulator.waitNSlots 1 
    -- | callEndpoint @"mint" h1 $ MintParams
    -- | { mpTokenName = name
    -- | , mpAmount    = 100
    -- |  }
    -- | void $ Emulator.waitNSlots 1  
    -- | callEndpoint @"publish" h2 tn
    -- |callEndpoint @"mint" h2 tn
    -- | void $ Emulator.waitNSlots 1
    -- | callEndpoint @"redeem" h3 tn2
    -- | void $ Emulator.waitNSlots 1