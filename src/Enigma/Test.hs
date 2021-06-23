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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- MODULE DEFINITION ----------------------------------------------------------

module Enigma.Test where

-- IMPORTS --------------------------------------------------------------------

import           Control.Monad              hiding (fmap)
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.Value               as Value
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..))
import           Wallet.Emulator.Wallet
import           Enigma.OffChain
import qualified Data.ByteString.Char8        as C

-- DEFINITIONS ----------------------------------------------------------------

-- | Dummy asset symbol for testing purposes.
assetSymbol :: CurrencySymbol
assetSymbol = "00FF00" 

-- | Helper function to convert CurrencySymbol to an String.
assetSymbolToString :: CurrencySymbol -> String
assetSymbolToString (CurrencySymbol x) = C.unpack x

-- | Configure and run our tests.
test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig $ Left $ Map.fromList [(Wallet i, v) | i <- [1]]

    v :: Value
    v = Ada.lovelaceValueOf             100_000_000 <>
        Value.singleton assetSymbol "A" 1           <>
        Value.singleton assetSymbol "B" 1           <>
        Value.singleton assetSymbol "C" 1           <>
        Value.singleton assetSymbol "D" 1           <>
        Value.singleton assetSymbol "F" 1           <>
        Value.singleton assetSymbol "X" 1

-- | Trace emulator function.
myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet (Wallet 1) endpoints

    -- Creates the test cube.
    callEndpoint @"create" h1 $ CreateParams {
        cpCubeId          = AssetClass (assetSymbol, "B"),
        cpStateMachineNft = AssetClass (assetSymbol, "C"),
        cpFirstReward     = AssetClass (assetSymbol, "A"),
        cpSecondReward    = AssetClass (assetSymbol, "D"),
        cpThirdReward     = AssetClass (assetSymbol, "F"),
        cpLastReward      = AssetClass (assetSymbol, "X"),
        cpAnswer01        = sha2_256 (C.pack ("A" ++ assetSymbolToString assetSymbol)),  
        cpAnswer02        = sha2_256 (C.pack ("B" ++ assetSymbolToString assetSymbol)), 
        cpAnswer03        = sha2_256 (C.pack ("C" ++ assetSymbolToString assetSymbol)), 
        cpAnswer04        = sha2_256 (C.pack ("D" ++ assetSymbolToString assetSymbol)),
        cpAnswer05        = sha2_256 (C.pack ("E" ++ assetSymbolToString assetSymbol)),
        cpAnswer06        = sha2_256 (C.pack ("F" ++ assetSymbolToString assetSymbol)),
        cpAnswer07        = sha2_256 (C.pack ("G" ++ assetSymbolToString assetSymbol)),
        cpAnswer08        = sha2_256 (C.pack ("H" ++ assetSymbolToString assetSymbol)),
        cpAnswer09        = sha2_256 (C.pack ("I" ++ assetSymbolToString assetSymbol)),
        cpAnswer10        = sha2_256 (C.pack ("J" ++ assetSymbolToString assetSymbol))
    }
    void $ Emulator.waitNSlots 1

    -- Solves the first puzzle
    callEndpoint @"solve" h1 $ SolveParams {
        spCubeId          = AssetClass (assetSymbol, "B"),
        spStateMachineNft = AssetClass (assetSymbol, "C"),
        spPuzzleIndex     = 0,
        spAnswer          = sha2_256(C.pack ("A" ++ assetSymbolToString assetSymbol))
    }
    void $ Emulator.waitNSlots 1

    -- Solves the second puzzle
    callEndpoint @"solve" h1 $ SolveParams {
        spCubeId          = AssetClass (assetSymbol, "B"),
        spStateMachineNft = AssetClass (assetSymbol, "C"),
        spPuzzleIndex     = 1,
        spAnswer          = sha2_256(C.pack ("B" ++ assetSymbolToString assetSymbol))
    }
    void $ Emulator.waitNSlots 1

    -- Solves the third puzzle
    callEndpoint @"solve" h1 $ SolveParams {
        spCubeId          = AssetClass (assetSymbol, "B"),
        spStateMachineNft = AssetClass (assetSymbol, "C"),
        spPuzzleIndex     = 2,
        spAnswer          = sha2_256(C.pack ("C" ++ assetSymbolToString assetSymbol))
    }
    void $ Emulator.waitNSlots 1

    -- Solves the fourth puzzle
    callEndpoint @"solve" h1 $ SolveParams {
        spCubeId          = AssetClass (assetSymbol, "B"),
        spStateMachineNft = AssetClass (assetSymbol, "C"),
        spPuzzleIndex     = 3,
        spAnswer          = sha2_256(C.pack ("D" ++ assetSymbolToString assetSymbol))
    }
    void $ Emulator.waitNSlots 1

    -- Solves the fifth puzzle
    callEndpoint @"solve" h1 $ SolveParams {
        spCubeId          = AssetClass (assetSymbol, "B"),
        spStateMachineNft = AssetClass (assetSymbol, "C"),
        spPuzzleIndex     = 4,
        spAnswer          = sha2_256(C.pack ("E" ++ assetSymbolToString assetSymbol))
    }
    void $ Emulator.waitNSlots 1

    -- Solves the sixth puzzle
    callEndpoint @"solve" h1 $ SolveParams {
        spCubeId          = AssetClass (assetSymbol, "B"),
        spStateMachineNft = AssetClass (assetSymbol, "C"),
        spPuzzleIndex     = 5,
        spAnswer          = sha2_256(C.pack ("F" ++ assetSymbolToString assetSymbol))
    }
    void $ Emulator.waitNSlots 1

    -- Gets the current puzzle index in the datum
    callEndpoint @"getCurrentLevel" h1 $ GetCurrentLevelParams
        { gclpCubeId          = AssetClass (assetSymbol, "B")
        , gclpStateMachineNft = AssetClass (assetSymbol, "C")
    }
    void $ Emulator.waitNSlots 1