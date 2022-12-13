{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Repositories.ProductGR (getProductsByShop, getProductsByOrder, getProductsWithOrdersId, getProductsByOrderId) where

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

instance GenericRepository Product

getProductsByShop :: Shop -> IO [Product]
getProductsByShop (Shop sId _ _) = filter (\x -> productShopId x == sId) <$> getList

getProductsByOrder :: Order -> IO [Product]
getProductsByOrder ord = getProductsByOrderId $ orderId ord

getProductsByOrderId :: Int -> IO [Product]
getProductsByOrderId sId =
  getList @ProductOrder >>= \out ->
    getList @Product >>= \allProduct ->
      return $
        getProductsByOrderId' (filter (\a -> opId a == sId) out) allProduct
  where
    getProductsByOrderId' :: [ProductOrder] -> [Product] -> [Product]
    getProductsByOrderId' (x : xs) allProduct = head (filter (\a -> productId a == poId x) allProduct) : getProductsByOrderId' xs allProduct
    getProductsByOrderId' [] _ = []

getProductsWithOrdersId :: IO [(Int, [Product])]
getProductsWithOrdersId =
  getList @Order >>= \out ->
    mapM
      ( \x ->
          getProductsByOrderId (orderId x) >>= \out ->
            return (orderId x, out)
      )
      out