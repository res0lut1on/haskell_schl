{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}

module Repositories.ProductGR (getProductsByShop, getProductsByOrder, getProductsWithOrdersId, getProductsByOrderId) where

import Data.Entities
import Repositories.GRepository
import ReadWrite.ReadWriteProduct ()
import Repositories.ProductOrderGR ()
import Repositories.OrderGR()

instance GenericRepository Product

getProductsByShop :: Shop -> IO [Product]
getProductsByShop (Shop sId _ _) = filter (\x -> productShopId x == sId) <$> getList

getProductsByOrder :: Order -> IO [Product]
getProductsByOrder ord = getProductsByOrderId $ orderId ord

getProductsByOrderId :: Int -> IO [Product]
getProductsByOrderId sId =
  do
    out <- getList @ProductOrder
    allProduct <- getList @Product 
    let ordWithProdId = filter (\a -> opId a == sId) out
    return $ getProductsByOrderId' ordWithProdId allProduct
  where
    getProductsByOrderId' :: [ProductOrder] -> [Product] -> [Product]
    getProductsByOrderId' (x : xs) allProduct = head (filter (\a -> productId a == poId x) allProduct) : getProductsByOrderId' xs allProduct
    getProductsByOrderId' [] _ = []

getProductsWithOrdersId :: IO [(Int, [Product])]
getProductsWithOrdersId =
  do
    out <- getList @Order 
    mapM
      ( \x ->
          do
            out <- getProductsByOrderId (orderId x)
            return (orderId x, out)
      )
      out