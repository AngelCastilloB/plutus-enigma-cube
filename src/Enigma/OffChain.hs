{-|
Module      : OffChain
Description : Contains the contract code that goes in the client.
Copyright   : (c) 2021 Angel Castillo
License     : Apache-2.0
Maintainer  : angel.castillob@protonmail.com
Stability   : experimental

OffChain contract code of the Enigma Cube game on the cardano blockchain.
-}

-- LANGUAGE EXTENSIONS --------------------------------------------------------

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NamedFieldPuns             #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Enigma.OffChain (
    CreateParams(..),
    SolveParams(..),
    GetCurrentLevelParams(..),
    endpoints 
) where

-- IMPORTS --------------------------------------------------------------------

import           Ledger     
import           Ledger.Value
import           Playground.Contract
import           Control.Monad        hiding (fmap)
import           Data.Text            (Text)
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Plutus.Contract      as Contract hiding (when)
import           Ledger.Constraints   as Constraints
import           Prelude              (Semigroup (..))
import qualified Prelude
import qualified PlutusTx
import qualified Data.Map             as Map
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Tx            as Tx
import Enigma.OnChain

-- DATA TYPES -----------------------------------------------------------------

-- | The parameters for the create cube end point.
data CreateParams = CreateParams {
        cpCubeId          :: !AssetClass, -- ^ The cube asset that can interact with this script.
        cpStateMachineNft :: !AssetClass, -- ^ The NFT that tracks the correct script output.
        cpFirstReward     :: !AssetClass, -- ^ The reward you get by solving the first 3 puzzles.
        cpSecondReward    :: !AssetClass, -- ^ The reward you get by solving the first 6 puzzles.
        cpThirdReward     :: !AssetClass, -- ^ The reward you get by solving the first 9 puzzles.
        cpLastReward      :: !AssetClass, -- ^ The reward you get by solving all the puzzles.
        cpAnswer01        :: !ByteString, -- ^ Hash of the first answer.
        cpAnswer02        :: !ByteString, -- ^ Hash of the second answer.
        cpAnswer03        :: !ByteString, -- ^ Hash of the third answer.
        cpAnswer04        :: !ByteString, -- ^ Hash of the fourth answer.
        cpAnswer05        :: !ByteString, -- ^ Hash of the fifth answer.
        cpAnswer06        :: !ByteString, -- ^ Hash of the sixth answer.
        cpAnswer07        :: !ByteString, -- ^ Hash of the seventh answer.
        cpAnswer08        :: !ByteString, -- ^ Hash of the eight answer.
        cpAnswer09        :: !ByteString, -- ^ Hash of the ninth answer.
        cpAnswer10        :: !ByteString  -- ^ The current puzzle index.
    } deriving (Show, Generic, FromJSON, ToJSON)

-- | The parameters for the solve cube end point.
data SolveParams = SolveParams { 
        spCubeId          :: !AssetClass, -- ^ The cube native toiken policy hash.
        spStateMachineNft :: !AssetClass, -- ^ The NFT that tracks this state machine.
        spPuzzleIndex     :: !Integer,    -- ^ The index of the puzzel to be solved (starting from 0).
        spAnswer          :: !ByteString  -- ^ The pre-image of the answer hash.
    } deriving (Generic, ToJSON, FromJSON)

-- | Gets the current puzzle this cube is on.
data GetCurrentLevelParams = GetCurrentLevelParams { 
        gclpCubeId          :: !AssetClass, -- ^ The cube asset that can interact with this script.
        gclpStateMachineNft :: !AssetClass  -- ^ The NFT that tracks the correct script output.
    } deriving (Generic, ToJSON, FromJSON)

-- DEFINITIONS ----------------------------------------------------------------

-- | Gets the output script that match the cube with the given parameters.
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

-- | Gets the output script that match the cube with the given parameters.
create :: forall w s. HasBlockchainActions s => CreateParams -> Contract w s Text ()
create createParams = do
    let datum = CubeDatum {
        firstReward        = cpFirstReward createParams,
        secondReward       = cpSecondReward createParams,
        thirdReward        = cpThirdReward createParams,
        lastReward         = cpLastReward createParams,
        firstAnswer        = sha2_256 (cpAnswer01 createParams),
        secondAnswer       = sha2_256 (cpAnswer02 createParams),
        thirdAnswer        = sha2_256 (cpAnswer03 createParams),
        fourthAnswer       = sha2_256 (cpAnswer04 createParams),
        fifthAnswer        = sha2_256 (cpAnswer05 createParams),
        sixthAnswer        = sha2_256 (cpAnswer06 createParams),
        seventhAnswer      = sha2_256 (cpAnswer07 createParams),
        eighthAnswer       = sha2_256 (cpAnswer08 createParams),
        ninthAnswer        = sha2_256 (cpAnswer09 createParams),
        tenthAnswer        = sha2_256 (cpAnswer10 createParams),
        currentPuzzleIndex = 0
    } 

    let cubeParams = CubeParameter {
        cubeId          = cpCubeId createParams,
        stateMachineNft = cpStateMachineNft createParams
    }

    let v = assetClassValue (stateMachineNft cubeParams) 1 <>
            assetClassValue (lastReward datum)           1 <> 
            assetClassValue (thirdReward datum)          1 <> 
            assetClassValue (secondReward datum)         1 <> 
            assetClassValue (firstReward datum)          1  
               
    let tx   = Constraints.mustPayToTheScript datum v

    ledgerTx <- submitTxConstraints (cubeInstance cubeParams) tx

    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @Prelude.String $ show datum
    logInfo @Prelude.String "Cube created."
    logInfo @Address $ cubeAddress cubeParams

-- | Builds the correct value for the new script output based on the puzzle index.
buildValue:: CubeParameter -> CubeDatum -> Integer -> Value -> Value
buildValue params datum index currentCuneValue
    | index <= 1 = firstStageBalance
    | index <= 4 = secondStageBalance
    | index <= 7 = thirdStageBalance
    | index <= 8 = lastStageBalance
    | index == 9 = mempty          -- if solve all puzzles claim all rewards
    | otherwise = currentCuneValue -- TODO: Define a better value for invalid cases
    where
        firstRewardPaid    = assetClassValue (firstReward datum)      1
        secondRewardPaid   = assetClassValue (secondReward datum)     1
        thirdRewardPaid    = assetClassValue (thirdReward datum)      1
        lastRewardPaid     = assetClassValue (lastReward datum)       1
        threadedNftPaid    = assetClassValue (stateMachineNft params) 1
        firstStageBalance  = firstRewardPaid <> secondRewardPaid <> thirdRewardPaid <> lastRewardPaid <> threadedNftPaid
        secondStageBalance = secondRewardPaid <> thirdRewardPaid <> lastRewardPaid <> threadedNftPaid  
        thirdStageBalance  = thirdRewardPaid <> lastRewardPaid <> threadedNftPaid  
        lastStageBalance   = lastRewardPaid <> threadedNftPaid  

-- | Tries to solve the ouzzle at the given index.
solve ::  forall w s. HasBlockchainActions s => SolveParams -> Contract w s Text ()
solve solveParams = do
    let cube = CubeParameter { 
        cubeId          = spCubeId solveParams,
        stateMachineNft = spStateMachineNft solveParams
    }
    pkh <- pubKeyHash <$> Contract.ownPubKey
    utxos <- utxoAt $ cubeAddress cube
    addressUtxos <- utxoAt $ pubKeyHashAddress pkh

    let constriants = Constraints.unspentOutputs utxos  <>
                      Constraints.unspentOutputs addressUtxos <>
                      Constraints.otherScript (Scripts.validatorScript (cubeInstance cube))  <>
                      Constraints.scriptInstanceLookups (cubeInstance cube) <> 
                      Constraints.ownPubKeyHash pkh

    m <- findCubeOutput cube
    case m of
        Nothing -> logInfo @String "Cube output not found for solve parameters "
        Just (_, _, dat) -> do
            let datum = dat { currentPuzzleIndex = spPuzzleIndex solveParams + 1 }
                redeemmer = CubeRedeemer (spPuzzleIndex solveParams) (spAnswer solveParams)
                totalValue  = Prelude.foldMap (Tx.txOutValue . Tx.txOutTxOut) utxos
                orefs       = fst <$> Map.toList utxos
                payToSelf   = assetClassValue (cubeId cube) 1 -- We must pay to outselves the cube so we can prove ownership of the cube.
                payToScript = (buildValue cube datum (spPuzzleIndex solveParams) totalValue)
                tx = mconcat [Constraints.mustSpendScriptOutput oref (Redeemer (PlutusTx.toData redeemmer)) | oref <- orefs] <>
                              Constraints.mustPayToTheScript datum payToScript <> 
                              Constraints.mustPayToPubKey pkh payToSelf
            ledgerTx <- submitTxConstraintsWith @Cube constriants tx
            void $ awaitTxConfirmed $ txId ledgerTx

-- | Gets the index of the current puzzle this cube is at.
getCurrentLevel :: forall w s. HasBlockchainActions s => GetCurrentLevelParams -> Contract w s Text ()
getCurrentLevel getLeveParams = do
    let cubeParams = CubeParameter {
        cubeId          = gclpCubeId getLeveParams,
        stateMachineNft = gclpStateMachineNft getLeveParams
    }
    -- Find current cube level.
    m <- findCubeOutput cubeParams
    case m of
        Nothing          -> logInfo @String  "game output not found"
        Just (_, _, dat) -> case dat of
            datum ->  logInfo @String $ "Current Level: " ++ show (currentPuzzleIndex datum)


-- ENDPOINTS ------------------------------------------------------------------

-- | Defines the cube schema.
type CubeSchema = BlockchainActions .\/ Endpoint "create" CreateParams .\/ Endpoint "solve" SolveParams .\/ Endpoint "getCurrentLevel" GetCurrentLevelParams

-- | Gets all the available endpoints.
endpoints :: Contract () CubeSchema Text ()
endpoints = (createEndpoint `select` solveEndpoint `select` getLevelEndpoint) >> endpoints
  where
    createEndpoint   = endpoint @"create"          >>= create
    solveEndpoint    = endpoint @"solve"           >>= solve
    getLevelEndpoint = endpoint @"getCurrentLevel" >>= getCurrentLevel