{-# LANGUAGE InstanceSigs #-}

module Util.CacheStyle (CacheStyle (..)) where

import Data.Entities
import Startup
import Control.Monad.State 

class CacheStyle a where
  getCache :: AppCache -> [a]

  setCache :: AppCache -> [a] -> AppCache

  updateCache ::  AppCache -> [a] -> App() 

instance CacheStyle Customer where
  getCache :: AppCache -> [Customer]
  getCache = customerCache

  setCache :: AppCache -> [Customer] -> AppCache
  setCache appstate xs = appstate {customerCache = xs}

instance CacheStyle Product where
  getCache :: AppCache -> [Product]
  getCache = productCache

  setCache :: AppCache -> [Product] -> AppCache
  setCache appstate xs = appstate {productCache = xs}

instance CacheStyle Order where
  getCache :: AppCache -> [Order]
  getCache = orderCache

  setCache :: AppCache -> [Order] -> AppCache
  setCache appstate xs = appstate {orderCache = xs}

instance CacheStyle Shop where
  getCache :: AppCache -> [Shop]
  getCache = shopCache

  setCache :: AppCache -> [Shop] -> AppCache
  setCache appstate xs = appstate {shopCache = xs}
