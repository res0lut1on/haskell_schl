{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.ProductOrderGR () where

import Data.Entities
import Repositories.GRepository
import ReadWrite.ReadWriteProductOrder ()
import Util.CacheStyle (CacheStyle (getCache, setCache))
import Startup

instance GenericRepository ProductOrder

