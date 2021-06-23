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

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ViewPatterns               #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Enigma.OnChain (
    CubeParameter(..),
    CubeDatum(..),
    CubeRedeemer(..),
    Cube,
    cubeInstance,
    cubeValidator,
    cubeAddress,
    cubeDatum
) where

-- IMPORTS --------------------------------------------------------------------

import           Data.Aeson           (FromJSON, ToJSON)
import           GHC.Generics         (Generic)
import           Ledger.Value
import           Ledger               hiding (singleton)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Contexts      as Validation

-- DATA TYPES -----------------------------------------------------------------

-- | The parameters for the enigma cubes state machine.
data CubeParameter = CubeParameter {
        cubeId          :: !AssetClass, -- ^ The cube native toiken policy hash.
        stateMachineNft :: !AssetClass  -- ^ The NFT that tracks this state machine.
    } deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''CubeParameter

-- | Datum data structure of the cube, the datum represents how many puzzles
--   The user has unlocked.
data CubeDatum = CubeDatum {
        firstReward        :: !AssetClass, -- ^ The reward you get by solving the first 3 puzzles.
        secondReward       :: !AssetClass, -- ^ The reward you get by solving the first 6 puzzles.
        thirdReward        :: !AssetClass, -- ^ The reward you get by solving the first 9 puzzles.
        lastReward         :: !AssetClass, -- ^ The reward you get by solving all the puzzles.
        firstAnswer        :: !ByteString, -- ^ Hash of the first answer.
        secondAnswer       :: !ByteString, -- ^ Hash of the second answer.
        thirdAnswer        :: !ByteString, -- ^ Hash of the third answer.
        fourthAnswer       :: !ByteString, -- ^ Hash of the fourth answer.
        fifthAnswer        :: !ByteString, -- ^ Hash of the fifth answer.
        sixthAnswer        :: !ByteString, -- ^ Hash of the sixth answer.
        seventhAnswer      :: !ByteString, -- ^ Hash of the seventh answer.
        eighthAnswer       :: !ByteString, -- ^ Hash of the eight answer.
        ninthAnswer        :: !ByteString, -- ^ Hash of the ninth answer.
        tenthAnswer        :: !ByteString, -- ^ Hash of the tenth answer.
        currentPuzzleIndex :: !Integer     -- ^ The current puzzle index.
    }
    deriving Show

PlutusTx.unstableMakeIsData ''CubeDatum

-- | The redeemer data structure for the cube. Its represented for the index
--   of the puzzle to solve plust the pre image of the answer.
data CubeRedeemer = CubeRedeemer {
        puzzleIndex::    !Integer,   -- ^ The index of the puzzel to be solved.
        answerPreimage:: !ByteString -- ^ The pre-image of the answer hash.
    }
    deriving Show

PlutusTx.unstableMakeIsData ''CubeRedeemer

-- | The cube script type. Sets the Redeemer and Datum types for this script.
data Cube 
instance Scripts.ScriptType Cube where
    type instance DatumType Cube = CubeDatum
    type instance RedeemerType Cube = CubeRedeemer

-- DEFINITIONS ----------------------------------------------------------------

-- | Maybe gets the datum from the transatcion output.
{-# INLINABLE cubeDatum #-}
cubeDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe CubeDatum
cubeDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromData d

-- | Checks that the given answer for the given puzzle index is correct.
{-# INLINABLE checkAnswer #-}
checkAnswer:: Integer -> ByteString -> CubeDatum -> Bool;
checkAnswer index answer datum
    | index == 0 = firstAnswer   datum == sha2_256 answer
    | index == 1 = secondAnswer  datum == sha2_256 answer
    | index == 2 = thirdAnswer   datum == sha2_256 answer
    | index == 3 = fourthAnswer  datum == sha2_256 answer
    | index == 4 = fifthAnswer   datum == sha2_256 answer
    | index == 5 = sixthAnswer   datum == sha2_256 answer
    | index == 6 = seventhAnswer datum == sha2_256 answer
    | index == 7 = eighthAnswer  datum == sha2_256 answer
    | index == 8 = ninthAnswer   datum == sha2_256 answer
    | index == 9 = tenthAnswer   datum == sha2_256 answer
    | otherwise = False

-- | Checks that the final balance of the new output matches the game rules.
{-# INLINABLE checkBalance #-}
checkBalance:: CubeParameter -> CubeDatum -> Integer -> Value -> Bool
checkBalance params datum index valueLockedByScript
    | index <= 1 = firstStageBalance
    | index <= 4 = secondStageBalance
    | index <= 7 = thirdStageBalance
    | index <= 8 = lastStageBalance                   
    | index >= 9 = True -- All balance can be taken out of the script.
    | otherwise  = False
    where
        firstRewardPaid    = assetClassValueOf valueLockedByScript (firstReward datum)      == 1
        secondRewardPaid   = assetClassValueOf valueLockedByScript (secondReward datum)     == 1
        thirdRewardPaid    = assetClassValueOf valueLockedByScript (thirdReward datum)      == 1
        lastRewardPaid     = assetClassValueOf valueLockedByScript (lastReward datum)       == 1
        threadedNftPaid    = assetClassValueOf valueLockedByScript (stateMachineNft params) == 1
        firstStageBalance  = firstRewardPaid  && secondRewardPaid && thirdRewardPaid && lastRewardPaid && threadedNftPaid
        secondStageBalance = secondRewardPaid && thirdRewardPaid  && lastRewardPaid  && threadedNftPaid  
        thirdStageBalance  = thirdRewardPaid  && lastRewardPaid   && threadedNftPaid  
        lastStageBalance   = lastRewardPaid   && threadedNftPaid  

-- | Creates the validator script for the outputs on this contract.
{-# INLINABLE mkGameValidator #-}
mkGameValidator :: CubeParameter -> CubeDatum -> CubeRedeemer -> ScriptContext -> Bool
mkGameValidator parameters oldDatum (CubeRedeemer puzzleIndex answer) ctx = 
    let oldPuzzleIndex         = currentPuzzleIndex oldDatum
        isRightPuzzleIndex     = oldPuzzleIndex == puzzleIndex
        isRightNextPuzzleIndex = (newDatumValue == (oldPuzzleIndex + 1))
        isRightAnswer          = checkAnswer puzzleIndex answer oldDatum
        isRightCube            = assetClassValueOf valueSpentByScript (cubeId parameters) == 1
        isBalanceRight         = checkBalance parameters oldDatum oldPuzzleIndex valueLockedByScript
    in traceIfFalse "Wrong puzzle index"            isRightPuzzleIndex && 
       traceIfFalse "Wrong next puzzle index state" isRightNextPuzzleIndex && 
       traceIfFalse "Wrong answer"                  isRightAnswer &&
       traceIfFalse "The right cube was not found"  isRightCube &&
       traceIfFalse "Wrong balance"                 isBalanceRight 
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "Expected exactly one cube output"

        newDatumValue :: Integer
        newDatumValue = case cubeDatum ownOutput (`findDatum` info) of
            Nothing -> traceError "Cube output datum not found"
            Just datum  -> currentPuzzleIndex datum

        valueSpentByScript :: Value
        valueSpentByScript = Validation.valueSpent info

        valueLockedByScript :: Value
        valueLockedByScript = Validation.valueLockedBy info (Validation.ownHash ctx)

-- | The script instance of the cube. It contains the mkGameValidator function
--   compiled to a Plutus core validator script.
cubeInstance :: CubeParameter -> Scripts.ScriptInstance Cube
cubeInstance cube = Scripts.validator @Cube
    ($$(PlutusTx.compile [|| mkGameValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cube) $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @CubeDatum @CubeRedeemer

-- | Gets the cube validator script that matches the given parameters.
cubeValidator :: CubeParameter -> Validator
cubeValidator = Scripts.validatorScript . cubeInstance

-- | Gets the cube address script that matches the cube with the given parameters.
cubeAddress :: CubeParameter -> Ledger.Address
cubeAddress = scriptAddress . cubeValidator

