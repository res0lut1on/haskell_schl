{-# OPTIONS_GHC -Wno-orphans #-}

module Mapping.ReadWriteProduct () where

import Data.Entities (Product)
import Mapping.ReadWriteEntityClass (ReadWriteDataEntity (..))

instance ReadWriteDataEntity Product