{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Data.DataBase.ReadWriteDataBase.ReadWriteDataBaseClass (ReadWriteDataBase (..)) where

import Control.Monad.IO.Class
import Control.Monad.RWS (ask)
import Data.Entities
import Data.RepEntity.BaseEntity
import qualified Data.Text as Text
import qualified Data.Text as TextPack
import Database.MSSQLServer.Query
import qualified Database.MSSQLServer.Query as MSSQL
import Startup

class (BaseEntity a, MSSQL.Row a, Show a) => ReadWriteDataBase a where
  addNewEnt :: a -> App ()
  addNewEnt cust =
    let q = "INSERT OR IGNORE INTO " <> entName cust <> " " <> entityField cust <> "VALUES " <> show cust
     in ask >>= \config ->
          liftIO $
            (MSSQL.sql (connectionString config) $ Text.pack q :: (IO RowCount))
              >>= \(RowCount rc) -> print rc

  -- tell "INSERt rc rows"

  -- writeAllDataEntity :: [a] -> App ()

  deleteEnt :: a -> App ()
  deleteEnt ent =
    let q = "DELETE FROM " <> entName ent <> "WHERE " <> entIdField ent <> " = " <> show (entId ent)
     in do
          config <- ask
          let res = MSSQL.sql (connectionString config) $ Text.pack (q)
          liftIO res

  updateEnt :: a -> App ()

  readAllDataEntity :: String -> App [a]
  readAllDataEntity ent =
    let q = "SELECT * FROM " <> ent
     in do
          config <- ask
          let res = MSSQL.sql (connectionString config) $ TextPack.pack (q)
          liftIO res

instance MSSQL.Row Customer

instance MSSQL.Row Order

instance MSSQL.Row Shop

instance MSSQL.Row ProductOrder

instance MSSQL.Row Product