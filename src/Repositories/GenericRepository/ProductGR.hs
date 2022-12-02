{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.GenericRepository.ProductGR () where

import Data.Entities
import Mapping.ReadWriteEntityClass
import Repositories.GenericRepository.GRepository
  ( GenericRepository,
  )

instance ReadWriteDataEntity Product 
-- я не понимаю, как решить проблему с импортом instance, 

instance GenericRepository Product