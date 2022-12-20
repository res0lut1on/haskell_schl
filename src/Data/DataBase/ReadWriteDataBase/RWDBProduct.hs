{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.DataBase.ReadWriteDataBase.RWDBProduct () where

import Control.Monad.RWS
import Data.DataBase.ReadWriteDataBase.ReadWriteDataBaseClass (ReadWriteDataBase (updateEnt))
import Data.Entities
import Data.RepEntity.BaseEntity
import qualified Data.Text as Text
import qualified Database.MSSQLServer.Query as MSSQL
import Startup

instance ReadWriteDataBase Data.Entities.Product where
  updateEnt :: Data.Entities.Product -> App ()
  updateEnt ent =
    let q =
          "UPDATE "
            <> entName ent
            <> " SET "
            <> "ShopID = "
            <> show (productShopId ent)
            <> ", ProductName = "
            <> show (productName ent)
            <> ", Price = "
            <> show (productPrice ent)
            <> ", Color = "
            <> show (productColor ent)
     in do
          config <- ask
          let res = MSSQL.sql (connectionString config) $ Text.pack (q)
          liftIO res
