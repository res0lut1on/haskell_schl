{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.ShopGR () where

import Data.DataBase.ReadWriteDataBase.RWDBShop ()
import Data.Entities
import Repositories.GRepository

instance GenericRepository Shop