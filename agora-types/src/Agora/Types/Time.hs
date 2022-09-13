{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- |Time functions for proposal phases.
module Agora.Types.Time (
  -- * Haskell-land
  ProposalTimingConfig (..),
  ProposalStartingTime (..),
  MaxTimeRangeWidth (..),
) where

import GHC.Generics (Generic)
import Plutus.V2.Ledger.Api (POSIXTime)
import PlutusTx qualified

--------------------------------------------------------------------------------

-- | Represents the starting time of the proposal.
--
--     @since 0.1.0
newtype ProposalStartingTime = ProposalStartingTime
  { getProposalStartingTime :: POSIXTime
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
    , -- | @since 0.1.0
      PlutusTx.UnsafeFromData
    )

-- | Configuration of proposal timings.
--
--     See: https://liqwid.notion.site/Proposals-589853145a994057aa77f397079f75e4#d25ea378768d4c76b52dd4c1b6bc0fcd
--
--     @since 0.1.0
data ProposalTimingConfig = ProposalTimingConfig
  { draftTime :: POSIXTime
  -- ^ "D": the length of the draft period.
  , votingTime :: POSIXTime
  -- ^ "V": the length of the voting period.
  , lockingTime :: POSIXTime
  -- ^ "L": the length of the locking period.
  , executingTime :: POSIXTime
  -- ^ "E": the length of the execution period.
  }
  deriving stock
    ( -- | @since 0.1.0
      Eq
    , -- | @since 0.1.0
      Show
    , -- | @since 0.1.0
      Generic
    )

PlutusTx.makeIsDataIndexed 'ProposalTimingConfig [('ProposalTimingConfig, 0)]

-- | Represents the maximum width of a 'PlutusLedgerApi.V1.Time.POSIXTimeRange'.
newtype MaxTimeRangeWidth = MaxTimeRangeWidth {getMaxWidth :: POSIXTime}
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
