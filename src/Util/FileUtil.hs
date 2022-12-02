{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.FileUtil
  ( readAllCustomers,
    readAllProducts,
    readAllShops,
    readAllOrders,
    readAllProductOrder,
    readEntityData,
    removeCust,
    initDataBase,
    addCust,
    addProd,
    addShp,
    addOrd,
    addProdOrd,
    updateCust,
    updateProd,
    updateShp,
    updateOrd,
    updateProdOrd,
    removeProdOrd,
    removeShp,
    removeOrd,
    removeProd,
    mappShopFromTxt,
    mappShopToTxt,
    mappCustFromTxt,
    mappCustToTxt,
    writeCustWhile,
    writeShopWhile,
    EntityContent,
    writeEntWhile,
    addEnt,
  )
where

import Control.Monad
import Data.CommonData (Color (White))
import Data.Context
import Data.Entities
import Data.RepEntity.BaseEntity
import GHC.IO.Handle
import Lib (remove, split)
import LibFold (add)
import Mapping.MappingTxt
import System.IO

----------------------------------------------------------------------------------------------------------------------

getRelativePathToHere :: FilePath
getRelativePathToHere = "/home/pineapple/doc/project_cabal/src/Data/ContextTxt"

getRelativePathToCustomers :: FilePath
getRelativePathToCustomers = getRelativePathToHere ++ "/customers.txt"

getRelativePathToShops :: FilePath
getRelativePathToShops = getRelativePathToHere ++ "/shops.txt"

getRelativePathToOrders :: FilePath
getRelativePathToOrders = getRelativePathToHere ++ "/orders.txt"

getRelativePathToProducts :: FilePath
getRelativePathToProducts = getRelativePathToHere ++ "/products.txt"

getRelativePathToProductOrder :: FilePath
getRelativePathToProductOrder = getRelativePathToHere ++ "/productOrder.txt"

----------------------------------------------------------------------------------------------------------------------

type CustomerTxtModel = String

type ProductTxtModel = String

type ShopTxtModel = String

type OrderTxtModel = String

type ProductOrderTxtModel = String

type EntityName = String

type EntityContent = String

----------------------------------------------------------------------------------------------------------------------

mappCustToTxt :: Customer -> CustomerTxtModel
mappCustToTxt (Customer cId n adr) = show cId ++ "|" ++ n ++ "|" ++ adr

mappProdToTxt :: Product -> ProductTxtModel
mappProdToTxt (Product prodId prodShopId name price color) = show prodId ++ "|" ++ show prodShopId ++ "|" ++ name ++ "|" ++ show price ++ "|" ++ show color

mappShopToTxt :: Shop -> ShopTxtModel
mappShopToTxt (Shop sId name address) = show sId ++ "|" ++ name ++ "|" ++ address

mappOrderToTxt :: Order -> OrderTxtModel
mappOrderToTxt (Order oId ordCustId ordNumId) = show oId ++ "|" ++ show ordCustId ++ "|" ++ ordNumId

mappProductOrderToTxt :: ProductOrder -> ProductOrderTxtModel
mappProductOrderToTxt (ProductOrder pOId prodOrdId ordProdId) = show pOId ++ "|" ++ show prodOrdId ++ "|" ++ show ordProdId

-----------------------------------------------------------------------------------------------------------------------

mappShopFromTxt :: ShopTxtModel -> Shop
mappShopFromTxt line =
  let shopSplit = split '|' line
   in Shop
        { shopId = read $ head shopSplit,
          shopName = head $ tail shopSplit,
          shopAddress = last shopSplit
        }

mappCustFromTxt :: CustomerTxtModel -> Customer
mappCustFromTxt line =
  let customersSplit = split '|' line
   in Customer
        { customerId = read $ head customersSplit,
          customerName = head $ tail customersSplit,
          customerAddress = last customersSplit
        }

mappOrderFromTxt :: OrderTxtModel -> Order
mappOrderFromTxt line =
  let orderSplit = split '|' line
   in Order
        { orderId = read $ head orderSplit,
          oCId = read $ head $ tail orderSplit,
          oNumber = last orderSplit
        }

mappProdOrdFromTxt :: ProductOrderTxtModel -> ProductOrder
mappProdOrdFromTxt line =
  let prodOrdSplit = split '|' line
   in ProductOrder
        { productOrderId = read $ head prodOrdSplit,
          poId = read $ head $ tail prodOrdSplit,
          opId = read $ last prodOrdSplit
        }

mappProdFromTxt :: ProductOrderTxtModel -> Product
mappProdFromTxt line =
  let productsSplit = split '|' line
   in Product (read $ head productsSplit) (read $ head $ tail productsSplit) (head $ tail $ tail productsSplit) (read $ head $ tail $ tail $ tail productsSplit) (read $ last productsSplit)

-------------------------------------------------------------------------------------------------------------------------

writeCustWhile :: Foldable t => t Customer -> IO ()
writeCustWhile cust =
  do
    outh <- openFile getRelativePathToCustomers WriteMode
    forM_
      cust
      ( \a ->
          do
            custloop a outh
      )
    hClose outh
  where
    custloop cust outh =
      do
        hPutStrLn outh (mappCustToTxt cust)
        return ()

writeProdWhile :: Foldable t => t Product -> IO ()
writeProdWhile prod =
  do
    outh <- openFile getRelativePathToProducts WriteMode
    forM_
      prod
      ( \a ->
          do
            prodloop a outh
      )
    hClose outh
  where
    prodloop prod outh =
      do
        hPutStrLn outh (mappProdToTxt prod)
        return ()

writeShopWhile :: Foldable t => t Shop -> IO ()
writeShopWhile shop =
  do
    outh <- openFile getRelativePathToShops WriteMode
    forM_
      shop
      ( \a ->
          do
            shoploop a outh
      )
    hClose outh
  where
    shoploop shop outh =
      do
        hPutStrLn outh (mappShopToTxt shop)
        return ()

writeOrderWhile :: Foldable t => t Order -> IO ()
writeOrderWhile ord =
  do
    outh <- openFile getRelativePathToOrders WriteMode
    forM_
      ord
      ( \a ->
          do
            orderloop a outh
      )
    hClose outh
  where
    orderloop ord outh =
      do
        hPutStrLn outh (mappOrderToTxt ord)
        return ()

writeProductOrder :: Foldable t => t ProductOrder -> IO ()
writeProductOrder prodOrd =
  do
    outh <- openFile getRelativePathToProductOrder WriteMode
    forM_
      prodOrd
      ( \a ->
          do
            productOrderloop a outh
      )
    hClose outh
  where
    productOrderloop prodOrd outh =
      do
        hPutStrLn outh (mappProductOrderToTxt prodOrd)
        return ()

--------------------------------------------------------------------------------------------------------------------------

writeEntWhile :: (BaseEntity a, MappEntity a) => [a] -> IO ()
writeEntWhile ents =
  do
    outh <- openFile (getRelativePathToHere ++ "/" ++ entName (head ents)) WriteMode
    forM_
      ents
      ( \a ->
          do
            entloop a outh
      )
    hClose outh
  where
    entloop es outh =
      do
        hPutStrLn outh (mappEntityTo es)
        return ()

readEntityData :: EntityName -> IO EntityContent
readEntityData entityName =
  do
    inh <- openFile (getRelativePathToHere ++ "/" ++ entityName ++ ".txt") ReadMode -- Тогда нужно будет создать только файл с именем сущности
    contents <- hGetContents inh
    putStrLn contents
    hClose inh
    return contents

instance MappEntity Customer where
  mappEntityTo :: Customer -> String
  mappEntityTo = mappCustToTxt
  mappEntityFrom :: String -> Customer
  mappEntityFrom = mappCustFromTxt

instance MappEntity Shop where
  mappEntityTo :: Shop -> String
  mappEntityTo = mappShopToTxt
  mappEntityFrom :: String -> Shop
  mappEntityFrom = mappShopFromTxt

instance MappEntity Product where
  mappEntityTo :: Product -> String
  mappEntityTo = mappProdToTxt
  mappEntityFrom :: String -> Product
  mappEntityFrom = mappProdFromTxt

instance MappEntity Order where
  mappEntityTo :: Order -> String
  mappEntityTo = mappOrderToTxt
  mappEntityFrom :: String -> Order
  mappEntityFrom = mappOrderFromTxt

instance MappEntity ProductOrder where
  mappEntityTo :: ProductOrder -> String
  mappEntityTo = mappProductOrderToTxt
  mappEntityFrom :: String -> ProductOrder
  mappEntityFrom = mappProdOrdFromTxt

---------------------------------------------------------------------------------------------------------------------------

readAllCustomers :: IO [Customer]
readAllCustomers =
  do
    contents <- readEntityData "customers"
    return $ map mappCustFromTxt (lines contents)

readAllProducts :: IO [Product]
readAllProducts =
  do
    contents <- readEntityData "products"
    return $ map mappProdFromTxt (lines contents)

readAllShops :: IO [Shop]
readAllShops =
  do
    contents <- readEntityData "shops"
    return $ map mappShopFromTxt (lines contents)

readAllOrders :: IO [Order]
readAllOrders =
  do
    contents <- readEntityData "orders"
    return $ map mappOrderFromTxt (lines contents)

readAllProductOrder :: IO [ProductOrder]
readAllProductOrder =
  do
    contents <- readEntityData "productOrder"
    return $ map mappProdOrdFromTxt (lines contents)

--------------------------------------------------------------------------------------------------------------------------------------------------------

addCust :: Customer -> IO ()
addCust cust = appendFile getRelativePathToCustomers (mappCustToTxt cust)

addProd :: Product -> IO ()
addProd prod = appendFile getRelativePathToProducts (mappProdToTxt prod)

addShp :: Shop -> IO ()
addShp shop = appendFile getRelativePathToShops (mappShopToTxt shop)

addOrd :: Order -> IO ()
addOrd o = appendFile getRelativePathToOrders (mappOrderToTxt o)

addProdOrd :: ProductOrder -> IO ()
addProdOrd po = appendFile getRelativePathToProductOrder (mappProductOrderToTxt po)

addEnt :: (MappEntity a, BaseEntity a) => a -> IO ()
addEnt ent = appendFile (getRelativePathToHere ++ "/" ++ entName ent) (mappEntityTo (entType ent))

--------------------------------------------------------------------------------------------------------------------------------------------------------

removeCust :: Int -> IO ()
removeCust item =
  do
    all <- readAllCustomers
    writeCustWhile $ Lib.remove (Customer {customerId = item}) all

removeProd :: Int -> IO ()
removeProd item =
  do
    all <- readAllProducts
    writeProdWhile $ Lib.remove (Product item 0 "" 0 White) all

removeOrd :: Int -> IO ()
removeOrd item =
  do
    all <- readAllOrders
    writeOrderWhile $ Lib.remove (Order {orderId = item}) all

removeShp :: Int -> IO ()
removeShp item =
  do
    all <- readAllShops
    writeShopWhile $ Lib.remove (Shop {shopId = item}) all

removeProdOrd :: Int -> IO ()
removeProdOrd item =
  do
    all <- readAllProductOrder
    writeProductOrder $ Lib.remove (ProductOrder {productOrderId = item}) all

------------------------------------------------------------------------------------------------------------------------------------------------------

updateCust :: Customer -> IO ()
updateCust new =
  do
    all <- readAllCustomers
    writeCustWhile (LibFold.add new (Lib.remove new all))

updateProd :: Product -> IO ()
updateProd new =
  do
    all <- readAllProducts
    writeProdWhile (LibFold.add new (Lib.remove new all))

updateShp :: Shop -> IO ()
updateShp new =
  do
    all <- readAllShops
    writeShopWhile (LibFold.add new (Lib.remove new all))

updateOrd :: Order -> IO ()
updateOrd new =
  do
    all <- readAllOrders
    writeOrderWhile (LibFold.add new (Lib.remove new all))

updateProdOrd :: ProductOrder -> IO ()
updateProdOrd new =
  do
    all <- readAllProductOrder
    writeProductOrder (LibFold.add new (Lib.remove new all))

------------------------------------------------------------------------------------------------------------------------------------------------------
-- sequence (mapM_ print [1,2,3,4])
-- "1|Michel|Gary, Indiana"
-- "2|Jimmy|Heston, UK"
-- "3|David|Cambrige, UK"

initDataBase :: IO ()
initDataBase =
  do
    writeCustWhile сustomers
    writeProdWhile products
    writeShopWhile shops
    writeOrderWhile orders
    writeProductOrder productOrder

readCustId :: Int -> IO Customer
readCustId custId =
  do
    inh <- openFile getRelativePathToCustomers ReadMode
    contents <- hGetContents inh
    putStrLn contents
    hClose inh
    return $ readCustomersById custId contents

readCustomersById :: Int -> String -> Customer
readCustomersById custId input =
  let allLines = map mappCustFromTxt (lines input)
   in head $ filter (\a -> customerId a == custId) allLines

checkCust :: Customer
checkCust =
  Customer
    { customerId = 99,
      customerName = "Bob",
      customerAddress = "Brugge"
    }

-- readEntity :: EntityName -> IO [a]
-- readEntity entityName
--   | entityName == "customers" =
--       do
--         readAllCustomers
--   | entityName == "products" =
--       do
--         readAllProducts
--   | entityName == "orders" =
--       do
--         readAllOrders
--   | entityName == "shops" =
--       do
--         readAllShops
--   | otherwise =
--       do
--         readAllProductOrder

-- readEntityData :: EntityName -> IO EntityContent
-- readEntityData entityName
--   | entityName == "customers" =
--       do
--         inh <- openFile getRelativePathToCustomers ReadMode
--         contents <- hGetContents inh
--         putStrLn contents
--         hClose inh
--         return contents
--   | entityName == "products" =
--       do
--         inh <- openFile getRelativePathToProducts ReadMode
--         contents <- hGetContents inh
--         putStrLn contents
--         hClose inh
--         return contents
--   | entityName == "orders" =
--       do
--         inh <- openFile getRelativePathToOrders ReadMode
--         contents <- hGetContents inh
--         putStrLn contents
--         hClose inh
--         return contents
--   | entityName == "shops" =
--       do
--         inh <- openFile getRelativePathToShops ReadMode
--         contents <- hGetContents inh
--         putStrLn contents
--         hClose inh
--         return contents
--   | otherwise =
--       do
--         inh <- openFile getRelativePathToProductOrder ReadMode
--         contents <- hGetContents inh
--         putStrLn contents
--         hClose inh
--         return contents