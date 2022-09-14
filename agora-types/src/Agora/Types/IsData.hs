{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Agora.Types.IsData (
  ProductIsData (..),
  EnumIsData (..),
) where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Maybe (Maybe (Nothing), fromJust)
import Data.Proxy (Proxy (Proxy))
import GHC.Base (Type)
import Generics.SOP (
  All,
  IsProductType,
  hcmap,
  hcollapse,
  hctraverse,
  mapIK,
  mapKI,
  productTypeFrom,
  productTypeTo,
  unI,
 )
import Generics.SOP qualified as SOP
import Plutus.V1.Ledger.Api (
  BuiltinData (BuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx (
  Data (List),
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  fromData,
  toData,
 )
import Prelude (Enum (fromEnum, toEnum), Integer, Integral (toInteger), Num (fromInteger), error, fmap, ($), (.), (<$>))

--------------------------------------------------------------------------------
-- ProductIsData

--  @since 1.1.0
newtype ProductIsData (a :: Type) = ProductIsData {unProductIsData :: a}

-- |
--  Generically convert a Product-Type to 'BuiltinData' with the 'List' repr.
--
--  @since 1.1.0
gProductToBuiltinData ::
  forall (a :: Type) (repr :: [Type]).
  (IsProductType a repr, All ToData repr) =>
  a ->
  BuiltinData
gProductToBuiltinData x =
  BuiltinData $ List $ hcollapse $ hcmap (Proxy @ToData) (mapIK toData) $ productTypeFrom x

-- |
--  Generically convert a Product-type from a 'BuiltinData' 'List' repr.
--
--  @since 1.1.0
gProductFromBuiltinData ::
  forall (a :: Type) (repr :: [Type]).
  (IsProductType a repr, All FromData repr) =>
  BuiltinData ->
  Maybe a
gProductFromBuiltinData (BuiltinData (List xs)) = do
  prod <- SOP.fromList @repr xs
  productTypeTo <$> hctraverse (Proxy @FromData) (unI . mapKI fromData) prod
gProductFromBuiltinData _ = Nothing

-- |
--  Unsafe version of 'gProductFromBuiltinData'.
--
--  @since 1.1.0
gProductFromBuiltinDataUnsafe ::
  forall (a :: Type) (repr :: [Type]).
  (IsProductType a repr, All UnsafeFromData repr) =>
  BuiltinData ->
  a
gProductFromBuiltinDataUnsafe (BuiltinData (List xs)) =
  let prod = fromJust $ SOP.fromList @repr xs
   in productTypeTo $
        runIdentity $
          hctraverse
            (Proxy @UnsafeFromData)
            (unI . mapKI (Identity . unsafeFromBuiltinData . BuiltinData))
            prod
gProductFromBuiltinDataUnsafe _ = error "invalid representation"

-- | @since 1.1.0
instance
  forall (a :: Type) (repr :: [Type]).
  (IsProductType a repr, All ToData repr) =>
  ToData (ProductIsData a)
  where
  toBuiltinData = coerce (gProductToBuiltinData @a)

-- | @since 1.1.0
instance
  forall (a :: Type) (repr :: [Type]).
  (IsProductType a repr, All UnsafeFromData repr) =>
  UnsafeFromData (ProductIsData a)
  where
  unsafeFromBuiltinData = coerce (gProductFromBuiltinDataUnsafe @a)

-- | @since 1.1.0
instance
  forall (a :: Type) (repr :: [Type]).
  (IsProductType a repr, All FromData repr) =>
  FromData (ProductIsData a)
  where
  fromBuiltinData = coerce (gProductFromBuiltinData @a)

--------------------------------------------------------------------------------
-- PEnumData

-- |
--  Wrapper for deriving 'ToData', 'FromData' using an Integer representation via 'Enum'.
--
--  @since 1.1.0
newtype EnumIsData (a :: Type) = EnumIsData a

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => ToData (EnumIsData a) where
  toBuiltinData = coerce $ toBuiltinData . toInteger . fromEnum @a

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => FromData (EnumIsData a) where
  fromBuiltinData = coerce $ fmap (toEnum @a . fromInteger) . fromBuiltinData @Integer

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => UnsafeFromData (EnumIsData a) where
  unsafeFromBuiltinData = coerce . toEnum @a . fromInteger . unsafeFromBuiltinData @Integer
