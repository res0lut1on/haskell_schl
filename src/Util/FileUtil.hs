{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.FileUtil
  ( readEntityData,
    initDataBase,
    mappShopFromTxt,
    mappShopToTxt,
    mappCustFromTxt,
    mappCustToTxt,
    EntityContent,
    writeEntWhile,
    addEnt,
  )
where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (MonadReader (ask))
import Data.Context
  ( orders,
    productOrder,
    products,
    shops,
    сustomers,
  )
import Data.Entities
import Data.MappingTxt
import Data.RepEntity.BaseEntity
import GHC.IO.Handle
import Lib (split)
import LibFold (toLowerCase)
import Startup
import System.IO

type CustomerTxtModel = String

type ProductTxtModel = String

type ShopTxtModel = String

type OrderTxtModel = String

type ProductOrderTxtModel = String

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

--------------------------------------------------------------------------------------------------------------------------

writeEntWhile :: (BaseEntity a, MappEntity a) => [a] -> App ()
writeEntWhile ents =
  ask >>= \config ->
    liftIO $
      openFile (filePath config ++ "/" ++ toLowerCase (entName (head ents)) ++ ".txt") WriteMode >>= \outh ->
        forM_
          ents
          (`entloop` outh)
          >> hClose outh
  where
    entloop es outh =
      hPutStrLn outh (mappEntityTo es)

readEntityData :: String -> App EntityContent
readEntityData eName =
  ask >>= \config ->
    liftIO $
      openFile (filePath config ++ "/" ++ toLowerCase eName ++ ".txt") ReadMode >>= \inh ->
        hGetContents inh >>= \contents ->
          putStrLn contents
            >> hClose inh
            >> return contents

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

--------------------------------------------------------------------------------------------------------------------------------------------------------

addEnt :: (MappEntity a, BaseEntity a) => a -> App ()
addEnt ent = ask >>= \config -> liftIO $ appendFile (filePath config ++ "/" ++ entName ent ++ ".txt") (mappEntityTo (entType ent))

--------------------------------------------------------------------------------------------------------------------------------------------------------

initDataBase :: App ()
initDataBase =
  writeEntWhile сustomers
    >> writeEntWhile products
    >> writeEntWhile shops
    >> writeEntWhile orders
    >> writeEntWhile productOrder
