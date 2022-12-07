{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.ProductOrderGR () where

import Data.Entities
import Repositories.GRepository
import ReadWrite.ReadWriteProductOrder ()

instance GenericRepository ProductOrder