{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Vote-lockable stake UTXOs holding GT.
module Agora.Types.Stake (
  -- * Haskell-land
  StakeDatum (..),
  StakeRedeemer (..),
  ProposalLock (..),
) where

import Agora.Types.IsData (ProductIsData (ProductIsData))
import Agora.Types.Proposal (ProposalId, ResultTag)
import Agora.Types.SafeMoney (GTTag)
import Data.Tagged (Tagged)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutus.V2.Ledger.Api (Credential)
import PlutusTx qualified

--------------------------------------------------------------------------------

-- | Locks that are stored in the stake datums for various purposes.
--
--     NOTE: Due to retracting votes always being possible,
--     this lock will only lock with contention on the proposal.
--
--     FIXME: Contention on Proposals could create contention
--     on voting which in turn creates contention on stakers.
--
--     Vaguely this is the dependency graph for this locking
--     interaction. Both the stake validator and the proposal
--     validator are only able to check for one another through
--     the datum belonging to the ST:
--
--     @
--     ┌─────────────────┐   ┌────────────────────┐
--     │ Stake Validator ├─┐ │ Proposal Validator │
--     └────────┬────────┘ │ └──────┬─────┬───────┘
--              │          │        │     │
--              │        ┌─┼────────┘     │
--              ▼        │ │              ▼
--     ┌──────────────┐  │ │ ┌─────────────────┐
--     │ Stake Policy │◄─┘ └►│ Proposal Policy │
--     └──────────────┘      └─────────────────┘
--     @
--
--     @since 0.1.0
data ProposalLock
  = -- | The stake was used to create a proposal.
    --
    --   This kind of lock is placed upon the creation of a proposal, in order
    --    to limit creation of proposals per stake.
    --
    --   See also: https://github.com/Liqwid-Labs/agora/issues/68
    --
    --   @since 0.2.0
    Created
      ProposalId
      -- ^ The identifier of the proposal.
  | -- | The stake was used to vote on a proposal.
    --
    --   This kind of lock is placed while voting on a proposal, in order to
    --    prevent depositing and withdrawing when votes are in place.
    --
    --   @since 0.2.0
    Voted
      ProposalId
      -- ^ The identifier of the proposal.
      ResultTag
      -- ^ The option which was voted on. This allows votes to be retracted.
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

PlutusTx.makeIsDataIndexed
  ''ProposalLock
  [ ('Created, 0)
  , ('Voted, 1)
  ]

-- | Haskell-level redeemer for Stake scripts.
--
--     @since 0.1.0
data StakeRedeemer
  = -- | Deposit or withdraw a discrete amount of the staked governance token.
    --   Stake must be unlocked.
    DepositWithdraw (Tagged GTTag Integer)
  | -- | Destroy a stake, retrieving its LQ, the minimum ADA and any other assets.
    --   Stake must be unlocked.
    Destroy
  | -- | Permit a Vote to be added onto a 'Agora.Proposal.Proposal'.
    --   This also adds a lock to the 'lockedBy' field. See 'ProposalLock'.
    --   This needs to be done in sync with casting a vote, otherwise
    --   it's possible for a lock to be permanently placed on the stake,
    --   and then the funds are lost.
    PermitVote
  | -- | Retract a vote, removing it from the 'lockedBy' field. See 'ProposalLock'.
    --   This action checks for permission of the 'Agora.Proposal.Proposal'. Finished proposals are
    --   always allowed to have votes retracted and won't affect the Proposal datum,
    --   allowing 'Stake's to be unlocked.
    RetractVotes
  | -- | The owner can consume stake if nothing is changed about it.
    --   If the proposal token moves, this is equivalent to the owner consuming it.
    WitnessStake
  | -- | The owner can delegate the stake to another user, allowing the
    --    delegate to vote on prooposals with the stake.
    DelegateTo Credential
  | -- | Revoke the existing delegation.
    ClearDelegate
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

PlutusTx.makeIsDataIndexed
  ''StakeRedeemer
  [ ('DepositWithdraw, 0)
  , ('Destroy, 1)
  , ('PermitVote, 2)
  , ('RetractVotes, 3)
  , ('WitnessStake, 4)
  , ('DelegateTo, 5)
  , ('ClearDelegate, 6)
  ]

-- | Haskell-level datum for Stake scripts.
--
--     @since 0.1.0
data StakeDatum = StakeDatum
  { stakedAmount :: Tagged GTTag Integer
  -- ^ Tracks the amount of governance token staked in the datum.
  --   This also acts as the voting weight for 'Agora.Proposal.Proposal's.
  , owner :: Credential
  -- ^ The hash of the public key this stake belongs to.
  --
  -- TODO Support for MultiSig/Scripts is tracked here:
  --      https://github.com/Liqwid-Labs/agora/issues/45
  , delegatedTo :: Maybe Credential
  -- ^ To whom this stake has been delegated.
  , lockedBy :: [ProposalLock]
  -- ^ The current proposals locking this stake. This field must be empty
  --   for the stake to be usable for deposits and withdrawals.
  }
  deriving stock
    ( -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.1.0
      SOP.Generic
    )
  deriving
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )
    via (ProductIsData StakeDatum)

--------------------------------------------------------------------------------
