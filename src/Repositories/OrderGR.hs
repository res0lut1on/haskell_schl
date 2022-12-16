{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.OrderGR (getOrderByCustomerId, getOrdersByCustomerId) where

import Control.Monad
import Control.Monad.Writer (MonadWriter (tell))
import Data.Entities
import ReadWrite.ReadWriteOrder ()
import Repositories.GRepository
import Startup
import Util.Utilities

instance GenericRepository Order

getOrderByCustomerId :: Int -> App Order
getOrderByCustomerId sId =
  let appArrEnt = filter (\x -> oCId x == sId) <$> getList
   in tell ["getOrderByCustomerId begin"]
        >> appArrEnt
        >>= ( isValidArr
                >=> (\res -> tell ["getOrderByCustomerId end"] >> return res)
            )

getOrdersByCustomerId :: Int -> App [Order]
getOrdersByCustomerId sId =
  let appArrEnt = filter (\x -> oCId x == sId) <$> getList
   in tell ["getOrdersByCustomerId begin"] >> appArrEnt >>= \res -> tell ["getOrdersByCustomerId end"] >> return res