{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- |Governor entity scripts acting as authority of entire system.
module Agora.Types.Governor (
  -- * Haskell-land
  GovernorDatum (..),
  GovernorRedeemer (..),
  Governor (..),
) where

import Agora.Types.Proposal (ProposalId, ProposalThresholds)
import Agora.Types.SafeMoney (GTTag)
import Agora.Types.Time (MaxTimeRangeWidth, ProposalTimingConfig)
import Data.Tagged (Tagged)
import GHC.Generics (Generic)
import Ledger.Value (AssetClass)
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx qualified
import PlutusTx.Integer (Integer)
import Prelude (Bounded, Enum, Show)

--------------------------------------------------------------------------------

-- | Datum for the Governor script.
--
--     @since 0.1.0
data GovernorDatum = GovernorDatum
  { proposalThresholds :: ProposalThresholds
  -- ^ Gets copied over upon creation of a 'Agora.Proposal.ProposalDatum'.
  , nextProposalId :: ProposalId
  -- ^ What tag the next proposal will get upon creating.
  , proposalTimings :: ProposalTimingConfig
  -- ^ The timing configuration for proposals.
  --   Will get copied over upon the creation of proposals.
  , createProposalTimeRangeMaxWidth :: MaxTimeRangeWidth
  -- ^ The maximum valid duration of a transaction that creats a proposal.
  , maximumProposalsPerStake :: Integer
  -- ^ The maximum number of unfinished proposals that a stake is allowed to be
  --   associated to.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed ''GovernorDatum [('GovernorDatum, 0)]

-- | Redeemer for Governor script. The governor has two primary
--     responsibilities:
--
--     1. The gating of Proposal creation.
--     2. The gating of minting authority tokens.
--
--     Parameters of the governor can also be mutated by an effect.
--
--     @since 0.1.0
data GovernorRedeemer
  = -- | Checks that a proposal was created lawfully, and allows it.
    CreateProposal
  | -- | Checks that a SINGLE proposal finished correctly,
    --   and allows minting GATs for each effect script.
    MintGATs
  | -- | Allows effects to mutate the parameters.
    MutateGovernor
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Enum
    , -- | @since 0.2.0
      Bounded
    )
  deriving
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )

-- | Parameters for creating Governor scripts.
--
--     @since 0.1.0
data Governor = Governor
  { gstOutRef :: TxOutRef
  -- ^ Referenced utxo will be spent to mint the GST.
  , gtClassRef :: Tagged GTTag AssetClass
  -- ^ Governance token of the system.
  , maximumCosigners :: Integer
  -- ^ Arbitrary limit for maximum amount of cosigners on a proposal.
  -- See `Agora.Proposal.proposalDatumValid`.
  }
  deriving stock
    ( -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Show
    )

--------------------------------------------------------------------------------
