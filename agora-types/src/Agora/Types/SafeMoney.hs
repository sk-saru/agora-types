{-# LANGUAGE FlexibleInstances #-}
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

import PlutusTx qualified
import PlutusTx.Builtins (matchData', mkI)
import PlutusTx.Prelude (Integer, Maybe (Just), fromMaybe)
import Prelude (Maybe (Nothing), const, error)

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

instance PlutusTx.FromData (Tagged GTTag Integer) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    Just
      ( Tagged
          ( fromMaybe
              (error "Parse Tagged error")
              (matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) Just (const Nothing))
          )
      )

instance PlutusTx.ToData (Tagged GTTag Integer) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Tagged i) = mkI i

instance PlutusTx.UnsafeFromData (Tagged GTTag Integer) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData d =
    Tagged
      ( fromMaybe
          (error "Parse Tagged error")
          (matchData' d (\_ _ -> Nothing) (const Nothing) (const Nothing) Just (const Nothing))
      )
