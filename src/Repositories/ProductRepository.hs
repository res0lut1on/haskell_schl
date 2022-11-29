module Repositories.ProductRepository
  ( getProducts,
    getProductById,
    getProductsByShop,
    removeProduct,
    updateProduct,
    addProduct,
    getProductsByOrderId,
    getProductsWithOrdersId,
    getProductsByOrder,
  )
where

import Data.Entities (Order (orderId), Product (..), ProductOrder (opId, poId), Shop (..), productColor, productId, productName, productPrice, productShopId)
import Util.FileUtil (addProd, readAllProductOrder, readAllProducts, removeProd, updateProd)
import Util.Utilities (maybeHead)
import Repositories.OrderRepository

getProducts :: IO [Product]
getProducts = readAllProducts

getProductById :: Int -> IO (Maybe Product)
getProductById searchId = maybeHead . filter (\x -> productId x == searchId) <$> getProducts

getProductsByShop :: Shop -> IO [Product]
getProductsByShop (Shop sId _ _) = filter (\x -> productShopId x == sId) <$> getProducts

getProductsByOrder :: Order -> IO [Product]
getProductsByOrder ord = getProductsByOrderId $ orderId ord

getProductsByOrderId :: Int -> IO [Product]
getProductsByOrderId sId =
  do
    out <- readAllProductOrder
    allProduct <- getProducts
    let ordWithProdId = filter (\a -> opId a == sId) out
    return $ getProductsByOrderId' ordWithProdId allProduct
  where
    getProductsByOrderId' :: [ProductOrder] -> [Product] -> [Product]
    getProductsByOrderId' (x : xs) allProduct = head (filter (\a -> productId a == poId x) allProduct) : getProductsByOrderId' xs allProduct
    getProductsByOrderId' [] _ = []

getProductsWithOrdersId :: IO [(Int, [Product])]
getProductsWithOrdersId =
  do
    out <- getOrd
    mapM
      ( \x ->
          do
            out <- getProductsByOrderId (orderId x)
            return (orderId x, out)
      )
      out

addProduct :: Product -> IO Int
addProduct item =
  do
    allItem <- readAllProducts
    let prevItem = last allItem
    let iId = productId prevItem + 1
    addProd
      ( Product iId (productShopId item) (productName item) (productPrice item) (productColor item)
      )
    return iId

removeProduct :: Int -> IO ()
removeProduct = removeProd

updateProduct :: Product -> IO ()
updateProduct = updateProd