{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.DataBase.ReadWriteDataBase.RWDBShop () where

import Control.Monad.RWS
import Data.DataBase.ReadWriteDataBase.ReadWriteDataBaseClass
  ( ReadWriteDataBase (updateEnt),
  )
import Data.Entities (Shop (shopAddress, shopName))
import Data.RepEntity.BaseEntity
import Data.Text as Text
import qualified Database.MSSQLServer.Query as MSSQL
import Startup
import Util.Utilities (toSqlQuotes)

instance ReadWriteDataBase Shop where
  updateEnt :: Shop -> App ()
  updateEnt ent =
    let q =
          "UPDATE "
            <> entName ent
            <> " SET "
            <> "ShopName = "
            <> toSqlQuotes (shopName ent)
            <> ", ShopAddress = "
            <> toSqlQuotes (shopAddress ent)
     in do
          config <- ask
          let res = MSSQL.sql (connectionString config) $ Text.pack (q)
          liftIO res
