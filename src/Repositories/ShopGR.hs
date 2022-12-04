{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.ShopGR () where

import Data.Entities
import ReadWrite.ReadWriteShop ()
import Repositories.GRepository

instance GenericRepository Shop