{-# OPTIONS_GHC -Wno-orphans #-}

module Mapping.ReadWriteOrder () where

import Data.Entities (Order)
import Mapping.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity Order