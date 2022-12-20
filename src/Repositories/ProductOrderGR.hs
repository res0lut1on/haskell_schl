{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.ProductOrderGR () where

import Data.DataBase.ReadWriteDataBase.RWDBProductOrder ()
import Data.Entities
import Repositories.GRepository

instance GenericRepository ProductOrder
