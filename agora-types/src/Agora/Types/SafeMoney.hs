{-# LANGUAGE OverloadedStrings #-}

module Agora.Types.SafeMoney (
  ADATag,
  GTTag,
  GovernorSTTag,
  StakeSTTag,
  ProposalSTTag,
  adaRef,
) where

import Data.Tagged (Tagged (Tagged))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

-- | Governance token.
--
--     @since 0.1.0
data GTTag

-- | ADA.
--
--     @since 0.1.0
data ADATag

-- | Governor ST token.
--
--     @since 0.1.0
data GovernorSTTag

-- | Stake ST token.
--
--     @since 0.1.0
data StakeSTTag

-- | Proposal ST token.
--
--     @since 0.1.0
data ProposalSTTag

-- | Resolves ada tags.
--
--     @since 0.1.0
adaRef :: Tagged ADATag AssetClass
adaRef = Tagged (AssetClass ("", ""))
