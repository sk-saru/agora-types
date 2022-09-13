{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Proposal scripts encoding effects that operate on the system.
module Agora.Types.Proposal (
  -- * Haskell-land

  -- Proposal (..),
  ProposalEffectGroup,
  ProposalDatum (..),
  ProposalRedeemer (..),
  ProposalStatus (..),
  ProposalThresholds (..),
  ProposalVotes (..),
  ProposalId (..),
  ResultTag (..),
  emptyVotesFor,
) where

import Agora.Types.SafeMoney (GTTag)
import Data.Map.Strict qualified as StrictMap
import Data.Tagged (Tagged)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutus.V2.Ledger.Api (
  Credential,
  DatumHash,
  ScriptHash,
  ValidatorHash,
 )
import PlutusTx qualified

import Agora.Types.IsData (EnumIsData (EnumIsData), ProductIsData (ProductIsData))
import Agora.Types.Time (ProposalStartingTime, ProposalTimingConfig)

--------------------------------------------------------------------------------
-- Haskell-land

-- | Identifies a Proposal, issued upon creation of a proposal. In practice,
--     this number starts at zero, and increments by one for each proposal.
--     The 100th proposal will be @'ProposalId' 99@. This counter lives
--     in the 'Agora.Governor.Governor'. See 'Agora.Governor.nextProposalId', and
--     'Agora.Governor.pgetNextProposalId'.
--
--     @since 0.1.0
newtype ProposalId = ProposalId {proposalTag :: Integer}
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )

-- | Encodes a result. Typically, for a Yes/No proposal, we encode it like this:
--
--     @
--     "No"  ~ 'ResultTag' 0
--     "Yes" ~ 'ResultTag' 1
--     @
--
--     @since 0.1.0
newtype ResultTag = ResultTag {getResultTag :: Integer}
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Ord
    , -- | @since 0.1.0
      Generic
    )
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )

-- | The "status" of the proposal. This is only useful for state transitions that
--     need to happen as a result of a transaction as opposed to time-based "periods".
--
--     See the note on wording & the state machine in the tech-design.
--
--     If the proposal is 'VotingReady', for instance, that doesn't necessarily
--     mean that voting is possible, as this also requires the timing to be right.
--
--     @since 0.1.0
data ProposalStatus
  = -- | A draft proposal represents a proposal that has yet to be realized.
    --
    --   In effect, this means one which didn't have enough LQ to be a full
    --   proposal, and needs cosigners to enable that to happen. This is
    --   similar to a "temperature check", but only useful if multiple people
    --   want to pool governance tokens together. If the proposal doesn't get to
    --   'VotingReady' on time, the proposal will __never__ be able to get
    --   voted on.
    Draft
  | -- | The proposal has/had enough GT cosigned in order to be a fully fledged
    --   proposal.
    --
    --   This means that once the timing requirements align,
    --   proposal will be able to be voted on.
    VotingReady
  | -- | The proposal has been voted on, and the votes have been locked
    --   permanently. The proposal now goes into a locking time after the
    --   normal voting time. After this, it's possible to execute the proposal.
    Locked
  | -- | The proposal has finished.
    --
    --   This can mean it's been voted on and completed, but it can also mean
    --   the proposal failed due to time constraints or didn't
    --   get to 'VotingReady' first.
    --
    --   At this stage, the 'votes' field of 'ProposalDatum' is frozen.
    --
    --   See 'AdvanceProposal' for documentation on state transitions.
    --
    --   TODO: The owner of the proposal may choose to reclaim their proposal.
    Finished
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    , -- | @since 0.2.0
      Enum
    , -- | @since 0.2.0
      Bounded
    )
  deriving anyclass
    ( -- | @since 0.2.0
      SOP.Generic
    )
  deriving
    ( -- | @since 0.1.0
      PlutusTx.FromData
    , -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )
    via (EnumIsData ProposalStatus)

-- | The threshold values for various state transitions to happen.
--     This data is stored centrally (in the 'Agora.Governor.Governor') and copied over
--     to 'Proposal's when they are created.
--
--     @since 0.1.0
data ProposalThresholds = ProposalThresholds
  { execute :: Tagged GTTag Integer
  -- ^ How much GT minimum must a particular 'ResultTag' accumulate for it to pass.
  , create :: Tagged GTTag Integer
  -- ^ How much GT required to "create" a proposal.
  --
  -- It is recommended this be a high enough amount, in order to prevent DOS from bad
  -- actors.
  , vote :: Tagged GTTag Integer
  -- ^ How much GT required to allow voting to happen.
  -- (i.e. to move into 'VotingReady')
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

PlutusTx.makeIsDataIndexed 'ProposalThresholds [('ProposalThresholds, 0)]

-- | Map which encodes the total tally for each result.
--     It's important that the "shape" is consistent with the shape of 'effects'.
--
--     e.g. if the 'effects' field looks like the following:
--
--     @[('ResultTag' 0, []), ('ResultTag' 1, [(vh, dh)])]@
--
--     Then 'ProposalVotes' needs be of the shape:
--
--     @[('ResultTag' 0, n), ('ResultTag' 1, m)]@
--
--     @since 0.1.0
newtype ProposalVotes = ProposalVotes
  { getProposalVotes :: StrictMap.Map ResultTag Integer
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving newtype
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )

-- | Create a 'ProposalVotes' that has the same shape as the 'effects' field.
--
--     @since 0.1.0
emptyVotesFor :: forall a. StrictMap.Map ResultTag a -> ProposalVotes
emptyVotesFor = ProposalVotes . StrictMap.mapWithKey (const . const 0)

-- | @since 0.3.0
type ProposalEffectGroup = StrictMap.Map ValidatorHash (DatumHash, Maybe ScriptHash)

-- | Haskell-level datum for Proposal scripts.
--
--     @since 0.1.0
data ProposalDatum = ProposalDatum
  { proposalId :: ProposalId
  -- ^ Identification of the proposal. Note that this map should be sorted in
  --    ascending order, and its keys should be unique.
  --
  -- TODO: could we encode this more efficiently?
  -- This is shaped this way for future proofing.
  -- See https://github.com/Liqwid-Labs/agora/issues/39
  , effects :: StrictMap.Map ResultTag ProposalEffectGroup
  -- ^ Effect lookup table. First by result, then by effect hash.
  , status :: ProposalStatus
  -- ^ The status the proposal is in.
  , cosigners :: [Credential]
  -- ^ Who created the proposal initially, and who cosigned it later.
  --
  -- This list should be sorted in **ascending** order.
  , thresholds :: ProposalThresholds
  -- ^ Thresholds copied over on initialization.
  , votes :: ProposalVotes
  -- ^ Vote tally on the proposal
  , timingConfig :: ProposalTimingConfig
  -- ^ Timing configuration copied over on initialization.
  , startingTime :: ProposalStartingTime
  -- ^ The time upon the creation of the proposal.
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 0.2.0
      SOP.Generic
    )
  deriving
    ( -- | @since 0.1.0
      PlutusTx.ToData
    , -- | @since 0.1.0
      PlutusTx.FromData
    )
    via (ProductIsData ProposalDatum)

-- | Haskell-level redeemer for Proposal scripts.
--
--     @since 0.1.0
data ProposalRedeemer
  = -- | Cast one or more votes towards a particular 'ResultTag'.
    Vote ResultTag
  | -- | Add one or more public keys to the cosignature list.
    --   Must be signed by those cosigning.
    --
    --   This is particularly used in the 'Draft' 'ProposalStatus',
    --   where matching 'Agora.Stake.Stake's can be called to advance the proposal,
    --   provided enough GT is shared among them.
    --
    --   This list should be sorted in ascending order.
    Cosign [Credential]
  | -- | Allow unlocking one or more stakes with votes towards particular 'ResultTag'.
    Unlock
  | -- | Advance the proposal, performing the required checks for whether that is legal.
    --
    --   These are roughly the checks for each possible transition:
    --
    --   === @'Draft' -> 'VotingReady'@:
    --
    --     1. The sum of all of the cosigner's GT is larger than the 'vote' field of 'ProposalThresholds'.
    --     2. The proposal's current time ensures 'isDraftPeriod'.
    --
    --   === @'VotingReady' -> 'Locked'@:
    --
    --     1. The sum of all votes is larger than 'execute'.
    --     2. The winning 'ResultTag' has more votes than all other 'ResultTag's.
    --     3. The proposal's current time ensures 'isVotingPeriod'.
    --
    --   === @'Locked' -> 'Finished'@:
    --
    --     1. The proposal's current time ensures 'isExecutionPeriod'.
    --     2. The transaction mints the GATs to the receiving effects.
    --
    --   === @* -> 'Finished'@:
    --
    --     If the proposal has run out of time for the current 'ProposalStatus', it will always be possible
    --     to transition into 'Finished' status, because it has expired (and failed).
    AdvanceProposal
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

-- | @since 0.1.0
PlutusTx.makeIsDataIndexed
  ''ProposalRedeemer
  [ ('Vote, 0)
  , ('Cosign, 1)
  , ('Unlock, 2)
  , ('AdvanceProposal, 3)
  ]
