{-# OPTIONS_GHC -Wno-orphans #-}

module Mapping.ReadWriteCustomer () where

import Data.Entities
import Mapping.ReadWriteEntityClass (ReadWriteDataEntity)

instance ReadWriteDataEntity Customer