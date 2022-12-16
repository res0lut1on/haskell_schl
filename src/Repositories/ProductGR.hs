{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.ProductGR (getProductsByShop, getProductsByOrderId, getProductsWithOrdersId) where

import Data.Entities
  ( Order (orderId),
    Product,
    ProductOrder (opId, poId),
    Shop (Shop),
    productId,
    productShopId,
  )
import ReadWrite.ReadWriteProduct ()
import Repositories.GRepository (GenericRepository (getList))
import Repositories.OrderGR ()
import Repositories.ProductOrderGR ()
import Startup
import Control.Monad.Writer (MonadWriter(tell))
import qualified Control.Monad

instance GenericRepository Product

getProductsByShop :: Shop -> App [Product]
getProductsByShop (Shop sId _ _) = tell ["getProductsByShop begin"] >> filter (\x -> productShopId x == sId) <$> getList >>= \res -> tell ["getProductsByShop end"] >> return res

getProductsByOrder :: Order -> App [Product]
getProductsByOrder ord = 
  let prd = getProductsByOrderId $ orderId ord
  in tell ["getProductsByOrder start"] >> prd >>= \res -> tell ["getProductsByOrder end"] 

getProductsByOrderId :: Int -> App [Product]
getProductsByOrderId sId =
  tell ["getProductsByOrderId begin"] >>
  getList @ProductOrder >>= \out ->
    getList @Product >>= \allProduct ->
      (\res -> tell ["getProductsByOrderId "] >> return res) (
        getProductsByOrderId' (filter (\a -> opId a == sId) out) allProduct )
  where
    getProductsByOrderId' :: [ProductOrder] -> [Product] -> [Product]
    getProductsByOrderId' (x : xs) allProduct = head (filter (\a -> productId a == poId x) allProduct) : getProductsByOrderId' xs allProduct
    getProductsByOrderId' [] _ = []

getProductsWithOrdersId :: App [(Int, [Product])]
getProductsWithOrdersId =
  tell ["getProductsWithOrdersId begin"] >>
  getList @Order >>= (mapM
   (\ x
      -> getProductsByOrderId (orderId x)
           >>= \ out -> return (orderId x, out))
   Control.Monad.>=>
     (\ res -> tell ["getProductsWithOrdersId end"] >> return res))