{-|
Module      : OnChain
Description : Contains the contract code that goes in the blockchain.
Copyright   : (c) 2021 Angel Castillo
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental

Onchain contract code of the Enigma Cube game on the cardano blockchain.
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Enigma.OnChain where

-- IMPORTS --------------------------------------------------------------------

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

-- DATA TYPES -----------------------------------------------------------------

-- | The string representation of a hash.
newtype HashedString = HashedString ByteString
    deriving newtype (PlutusTx.IsData, Eq)
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString ByteString
    deriving newtype (PlutusTx.IsData, Eq)
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''ClearString

-- | The parameters for the enigma cubes state machine.
data CubeParameter = CubeParameter
    { cubeId          :: !AssetClass -- ^ The cube native toiken policy hash.
    , stateMachineNft :: !AssetClass -- ^ The NFT that tracks this state machine.
    , firstReward     :: !AssetClass -- ^ The reward you get by solving the first 3 puzzles.
    , secondReward    :: !AssetClass -- ^ The reward you get by solving the first 6 puzzles.
    , thirdReward     :: !AssetClass -- ^ The reward you get by solving the first 9 puzzles.
    , lastReard       :: !AssetClass -- ^ The reward you get by solving the first 9 puzzles.
    , firstAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , secondAnswer    :: !HashedString   -- ^ Hash of the first answer.
    , thirdAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , fourthAnswer    :: !HashedString   -- ^ Hash of the first answer.
    , fifthAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , sixthAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , seventhAnswer   :: !HashedString   -- ^ Hash of the first answer.
    , eightAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , ninethAnswer    :: !HashedString   -- ^ Hash of the first answer.
    , tenthAnswer     :: !HashedString   -- ^ Hash of the first answer.
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''CubeParameter

-- | Datum data structure of the cube, the datum represents how many puzzles
--   The user has unlocked.
data CubeDatum = CubeDatum Integer
    deriving Show

instance Eq CubeDatum where
    {-# INLINABLE (==) #-}
    CubeDatum index == CubeDatum index' = (index == index')

PlutusTx.unstableMakeIsData ''CubeDatum

-- | The redeemer data structure for the cube. Its represented for the index
--   of the puzzle to solve plust the pre image of the answer.
data CubeRedeemer = CubeRedeemer Integer HashedString
    deriving Show

PlutusTx.unstableMakeIsData ''CubeRedeemer

-- DEFINITIONS ----------------------------------------------------------------

{-# INLINABLE validatorHashOutputsAt #-}
validatorHashOutputsAt :: ValidatorHash -> TxInfo -> [Value]
validatorHashOutputsAt pk p =
    let flt V.TxOut{V.txOutAddress = Address (ScriptCredential pk') _, V.txOutValue} | pk == pk' = Just txOutValue
        flt _                             = Nothing
    in mapMaybe flt (txInfoOutputs p)

{-# INLINABLE valuePaidToScript #-}
-- | Get the total value paid to a public key address by a pending transaction.
valuePaidToScript :: TxInfo -> ValidatorHash -> Value
valuePaidToScript ptx pkh = mconcat (validatorHashOutputsAt pkh ptx)

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE cubeDatum #-}
cubeDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe CubeDatum
cubeDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d

{-# INLINABLE checkAnswer #-}
checkAnswer:: Integer -> HashedString -> CubeParameter -> Bool;
checkAnswer index (HashedString answer) params
					| index == 0 = (firstAnswer params) == HashedString (sha2_256 answer)
                    | index == 1 = (secondAnswer params) == HashedString (sha2_256 answer)
                    | index == 2 = (thirdAnswer params) == HashedString (sha2_256 answer)
                    | index == 3 = (fourthAnswer params) == HashedString (sha2_256 answer)
                    | index == 4 = (fifthAnswer params) == HashedString (sha2_256 answer)
                    | index == 5 = (sixthAnswer params) == HashedString (sha2_256 answer)
                    | index == 6 = (seventhAnswer params) == HashedString (sha2_256 answer)
                    | index == 7 = (eightAnswer params) == HashedString (sha2_256 answer)
                    | index == 8 = (ninethAnswer params) == HashedString (sha2_256 answer)
                    | index == 9 = (tenthAnswer params) == HashedString (sha2_256 answer)
					| otherwise = False

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: CubeParameter -> CubeDatum -> CubeRedeemer -> ScriptContext -> Bool
mkGameValidator parameters (CubeDatum oldPuzzleIndex) (CubeRedeemer puzzleIndex answer) ctx = 
    let isRightPuzzleIndex     = oldPuzzleIndex == puzzleIndex
        isRightNextPuzzleIndex = (newDatumValue == (oldPuzzleIndex + 1))
        isRightAnswer          = checkAnswer puzzleIndex answer parameters
        isBalanceRight         = True
    in traceIfFalse "Wrong puzzle index"            isRightPuzzleIndex && 
       traceIfFalse "Wrong next puzzle index state" isRightNextPuzzleIndex && 
       traceIfFalse "Wrong answer"                  isRightAnswer &&
       traceIfFalse "Wrong balance"                 isBalanceRight 
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "Cube input missing"
            Just i  -> txInInfoResolved i

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "Expected exactly one cube output"

        newDatumValue :: Integer
        newDatumValue = case cubeDatum ownOutput (`findDatum` info) of
            Nothing -> traceError "Cube output datum not found"
            Just (CubeDatum index)  -> index

data Cube
instance Scripts.ScriptType Cube where
    type instance DatumType Cube = CubeDatum
    type instance RedeemerType Cube = CubeRedeemer

cubeInst :: CubeParameter -> Scripts.ScriptInstance Cube
cubeInst cube = Scripts.validator @Cube
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode cube)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CubeDatum @CubeRedeemer

cubeValidator :: CubeParameter -> Validator
cubeValidator = Scripts.validatorScript . cubeInst

cubeAddress :: CubeParameter -> Ledger.Address
cubeAddress = scriptAddress . cubeValidator

findCubeOutput :: HasBlockchainActions s => CubeParameter -> Contract w s Text (Maybe (TxOutRef, TxOutTx, CubeDatum))
findCubeOutput cube = do
    utxos <- utxoAt $ cubeAddress cube
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- cubeDatum (txOutTxOut o) (`Map.lookup` txData (txOutTxTx o))
        return (oref, o, dat)
  where
    f :: (TxOutRef, TxOutTx) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ txOutTxOut o) (stateMachineNft cube) == 1


data CreateParams = CreateParams
    { cubeIdCurrency  :: !CurrencySymbol 
    , cubeIdTokenName :: !TokenName 
    , stateMachineNftCurrency  :: !CurrencySymbol 
    , stateMachineNftTokenName :: !TokenName 
    , firstRewardCurrency  :: !CurrencySymbol 
    , firstRewardTokenName :: !TokenName 
    , secondRewardCurrency  :: !CurrencySymbol 
    , secondRewardTokenName :: !TokenName 
    , thirdRewardCurrency  :: !CurrencySymbol 
    , thirdRewardTokenName :: !TokenName 
    , lastReardCurrency  :: !CurrencySymbol 
    , lastReardTokenName :: !TokenName 
    , fiAnswer     :: !ByteString   -- ^ Hash of the first answer.
    , seAnswer    :: !ByteString   -- ^ Hash of the first answer.
    , thAnswer     :: !ByteString   -- ^ Hash of the first answer.
    , foAnswer    :: !ByteString   -- ^ Hash of the first answer.
    , fvAnswer     :: !ByteString   -- ^ Hash of the first answer.
    , siAnswer     :: !ByteString   -- ^ Hash of the first answer.
    , svAnswer   :: !ByteString   -- ^ Hash of the first answer.
    , eiAnswer     :: !ByteString   -- ^ Hash of the first answer.
    , niAnswer    :: !ByteString   -- ^ Hash of the first answer.
    , teAnswer     :: !ByteString   -- ^ Hash of the first answer.
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

create :: forall w s. HasBlockchainActions s => CreateParams -> Contract w s Text ()
create createParams = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let datum      = CubeDatum 0
    let cubeParams = CubeParameter
            { cubeId          = AssetClass (cubeIdCurrency createParams, cubeIdTokenName createParams)
            , stateMachineNft = AssetClass (stateMachineNftCurrency createParams, stateMachineNftTokenName createParams)
            , firstReward     = AssetClass (firstRewardCurrency createParams, firstRewardTokenName createParams)
            , secondReward    = AssetClass (secondRewardCurrency createParams, secondRewardTokenName createParams)
            , thirdReward     = AssetClass (thirdRewardCurrency createParams, thirdRewardTokenName createParams)
            , lastReard       = AssetClass (lastReardCurrency createParams, lastReardTokenName createParams)
            , firstAnswer     = HashedString (sha2_256 (fiAnswer createParams))
            , secondAnswer    = HashedString (sha2_256 (seAnswer createParams))
            , thirdAnswer     = HashedString (sha2_256 (thAnswer createParams))
            , fourthAnswer    = HashedString (sha2_256 (foAnswer createParams))
            , fifthAnswer     = HashedString (sha2_256 (fvAnswer createParams))
            , sixthAnswer     = HashedString (sha2_256 (siAnswer createParams))
            , seventhAnswer   = HashedString (sha2_256 (svAnswer createParams))
            , eightAnswer     = HashedString (sha2_256 (eiAnswer createParams))
            , ninethAnswer    = HashedString (sha2_256 (niAnswer createParams))
            , tenthAnswer     = HashedString (sha2_256 (teAnswer createParams))
            }
    let v    = assetClassValue (stateMachineNft cubeParams) 1 <>
               assetClassValue (lastReard cubeParams)       1 <> 
               assetClassValue (thirdReward cubeParams)     1 <> 
               assetClassValue (secondReward cubeParams)    1 <> 
               assetClassValue (firstReward cubeParams)     1  
               
    let tx   = Constraints.mustPayToTheScript datum v
    ledgerTx <- submitTxConstraints (cubeInst cubeParams) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @Prelude.String $ show datum
    logInfo @Prelude.String $ "Cube created!"

    logInfo @Address $ cubeAddress cubeParams
    
    logInfo @ByteString $ fiAnswer createParams
    logInfo @ByteString $ seAnswer createParams
    logInfo @ByteString $ thAnswer createParams
    logInfo @ByteString $ foAnswer createParams
    logInfo @ByteString $ fvAnswer createParams
    logInfo @ByteString $ siAnswer createParams
    logInfo @ByteString $ svAnswer createParams
    logInfo @ByteString $ eiAnswer createParams
    logInfo @ByteString $ niAnswer createParams
    logInfo @ByteString $ teAnswer createParams

    void $ awaitSlot 10

    -- Find current cube level.
    m <- findCubeOutput cubeParams
    case m of
        Nothing             -> logInfo @String  "game output not found"
        Just (oref, o, dat) -> case dat of
            CubeDatum index ->  logInfo @String $ show index ++ show " " ++ show oref ++ " " ++  show o

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

mint :: forall w s. HasBlockchainActions s => MintParams -> Contract w s Text ()
mint (MintParams name amount) = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let val     = Value.singleton Seals.sealsSymbol name amount
        lookups = Constraints.monetaryPolicy Seals.policy
        tx      = (Constraints.mustForgeValue val) <> (Constraints.mustPayToPubKey pkh val)  
    ledgerTx <- submitTxConstraintsWith @Cube lookups tx

    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo $ show val

--    fst firstReward snd firstReward
--    AssetClass { unAssetClass :: (CurrencySymbol, TokenName) }

data SolveParams = SolveParams
    { sCubeId          :: !AssetClass -- ^ The cube native toiken policy hash.
    , sStateMachineNft :: !AssetClass -- ^ The NFT that tracks this state machine.
    , sFirstReward     :: !AssetClass -- ^ The reward you get by solving the first 3 puzzles.
    , sSecondReward    :: !AssetClass -- ^ The reward you get by solving the first 6 puzzles.
    , sThirdReward     :: !AssetClass -- ^ The reward you get by solving the first 9 puzzles.
    , sLastReard       :: !AssetClass -- ^ The reward you get by solving the first 9 puzzles.
    , sFirstAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , sSecondAnswer    :: !HashedString   -- ^ Hash of the first answer.
    , sThirdAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , sFourthAnswer    :: !HashedString   -- ^ Hash of the first answer.
    , sFifthAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , sSixthAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , sSeventhAnswer   :: !HashedString   -- ^ Hash of the first answer.
    , sEightAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , sNinethAnswer    :: !HashedString   -- ^ Hash of the first answer.
    , sTenthAnswer     :: !HashedString   -- ^ Hash of the first answer.
    , sPuzzleIndex :: !Integer
    , sAnswer    :: !ByteString
    } deriving (Generic, ToJSON, FromJSON)


-- | The "redeem" contract endpoint.
solve ::  forall w s. HasBlockchainActions s => SolveParams -> Contract w s Text ()
solve solveParams = do

    let cube = CubeParameter { cubeId          = sCubeId solveParams
    , stateMachineNft = sStateMachineNft solveParams
    , firstReward     = sFirstReward solveParams
    , secondReward    = sSecondReward solveParams
    , thirdReward     = sThirdReward solveParams
    , lastReard       = sLastReard solveParams
    , firstAnswer     = sFirstAnswer solveParams
    , secondAnswer    = sSecondAnswer solveParams
    , thirdAnswer     = sThirdAnswer solveParams
    , fourthAnswer    = sFourthAnswer solveParams
    , fifthAnswer     = sFifthAnswer solveParams
    , sixthAnswer     = sSixthAnswer solveParams
    , seventhAnswer   = sSeventhAnswer solveParams
    , eightAnswer     = sEightAnswer solveParams
    , ninethAnswer    = sNinethAnswer solveParams
    , tenthAnswer     = sTenthAnswer solveParams
    }

    utxos <- utxoAt $ cubeAddress cube
    let cubeDatum = CubeDatum ((sPuzzleIndex solveParams) + 1)
    let cubeRedeemer = CubeRedeemer (sPuzzleIndex solveParams) (HashedString (sAnswer solveParams))
    let totalValue = Prelude.foldMap (Tx.txOutValue . Tx.txOutTxOut) utxos
    let orefs   = fst <$> Map.toList utxos

    Contract.logInfo $ show cubeDatum
    Contract.logInfo $ show cubeRedeemer
    Contract.logInfo $ show totalValue

    let lookups =  Constraints.unspentOutputs utxos <>
                   Constraints.otherScript (Scripts.validatorScript (cubeInst cube))  <>
                   Constraints.scriptInstanceLookups (cubeInst cube) <> 
                   Constraints.monetaryPolicy Seals.policy
        tx      = mconcat [Constraints.mustSpendScriptOutput oref (Redeemer  (PlutusTx.toData cubeRedeemer)) | oref <- orefs]
           <> (Constraints.mustPayToTheScript cubeDatum totalValue)  
    ledgerTx <- submitTxConstraintsWith @Cube lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    h3 <- activateContractWallet (Wallet 3) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = "A"
        , mpAmount    = 12
        }
    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints

    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = "B"
        , mpAmount    = 12
        }
    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints

    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = "C"
        , mpAmount    = 12
        }
    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints

    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = "D"
        , mpAmount    = 12
        }
    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints

    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = "F"
        , mpAmount    = 12
        }
    void $ Emulator.waitNSlots 1 

    h3 <- activateContractWallet (Wallet 3) endpoints

    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = "X"
        , mpAmount    = 12
        }
    void $ Emulator.waitNSlots 1 

    h1 <- activateContractWallet (Wallet 1) endpoints
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
    void $ Emulator.waitNSlots 1
    h1 <- activateContractWallet (Wallet 1) endpoints

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
    h1 <- activateContractWallet (Wallet 1) endpoints
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
        , sPuzzleIndex     = 1
        , sAnswer          = sha2_256(C.pack ("B" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
    }
    void $ Emulator.waitNSlots 1 
    h1 <- activateContractWallet (Wallet 1) endpoints

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
        , sPuzzleIndex     = 2
        , sAnswer          = sha2_256(C.pack ("C" ++ "c6f5d508c16c5602dbbd5c4c5618064f66b31a2e99002b70d116a645343c137e") )
    }
    void $ Emulator.waitNSlots 1 
type CubeSchema = BlockchainActions .\/ Endpoint "create" CreateParams .\/ Endpoint "mint" MintParams .\/ Endpoint "solve" SolveParams

endpoints :: Contract () CubeSchema Text ()
endpoints = (first `select` second `select` third) >> endpoints
  where
    first  = endpoint @"create"  >>= create
    second = endpoint @"mint" >>= mint
    third  = endpoint @"solve" >>= solve

--mkSchemaDefinitions ''CubeSchema
$(mkKnownCurrencies [])
