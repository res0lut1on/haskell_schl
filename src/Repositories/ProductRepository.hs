module Repositories.ProductRepository
  ( getProducts,
    getProductById,
    getProductsByShop,
    getProductsByOrderId,
    getProductsByOrder,
  )
where

import Data.Context (products)
import Data.Entities (Customer, Order (..), Product (..), Shop (..), productId, productShopId)
import Utilities (maybeHead)

getProducts :: [Product]
getProducts = products

getProductById :: Int -> Maybe Product
getProductById searchId = maybeHead $ filter (\x -> productId x == searchId) getProducts

getProductsByShop :: Shop -> [Product]
getProductsByShop (Shop sId _ _) = filter (\x -> productShopId x == sId) getProducts

getProductsByOrderId :: Int -> [Product]
getProductsByOrderId sId = filter (\x -> productId x == sId) getProducts

getProductsByOrder :: Order -> [Product]
getProductsByOrder (Order _ sId _) = filter (\x -> productId x == sId) getProducts
