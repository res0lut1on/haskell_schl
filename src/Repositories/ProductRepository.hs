module Repositories.ProductRepository
  ( getProducts,
    getProductById,
    getProductsByShop,
    getProductsByOrderId,
    getProductsByOrder,
  )
where

import Data.Context (products)
import Data.Entities (Order (..), Product (..), Shop (..), productId, productShopId)

getProducts :: [Product]
getProducts = products

getProductById :: Int -> Product
getProductById searchId = head $ filter (\x -> productId x == searchId) products

getProductsByShop :: Shop -> [Product]
getProductsByShop (Shop sId _ _) = filter (\x -> productShopId x == sId) products

getProductsByOrderId :: Int ->  [Product]
getProductsByOrderId sId = filter (\x -> productId x == sId) products

getProductsByOrder :: Order -> [Product]
getProductsByOrder (Order _ sId _) = filter (\x -> productId x == sId) products