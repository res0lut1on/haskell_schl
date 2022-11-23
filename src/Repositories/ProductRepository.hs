module Repositories.ProductRepository
  ( getProducts,
    getProductById,
    getProductsByShop,
    getProductsByOrderId,
    getProductsByOrder,
  )
where

import Data.Context (productOrder, products)
import Data.Entities (Order (..), Product (..), ProductOrder (opId, poId), Shop (..), productId, productShopId)
import Utilities (maybeHead)

getProducts :: [Product]
getProducts = products

getProductById :: Int -> Maybe Product
getProductById searchId = maybeHead $ filter (\x -> productId x == searchId) getProducts

getProductsByShop :: Shop -> [Product]
getProductsByShop (Shop sId _ _) = filter (\x -> productShopId x == sId) getProducts

getProductsByOrder :: Order -> [Product]
getProductsByOrder (Order _ sId _) = getProductsByOrderId sId

getProductsByOrderId :: Int -> [Product]
getProductsByOrderId sId = getProductsByOrderId' (filter (\a -> opId a == sId) productOrder)
  where
    getProductsByOrderId' (x : xs) =
      let prod = (getProductById $ poId x)
       in case prod of
            Just value -> value : getProductsByOrderId' xs
    getProductsByOrderId' [] = []
