{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.DataBase.ReadWriteDataBase.RWDBOrder () where

import Control.Monad.RWS
import Data.DataBase.ReadWriteDataBase.ReadWriteDataBaseClass
import Data.Entities (Order (oCId, oNumber))
import Data.RepEntity.BaseEntity
import Data.Text as Text
import qualified Database.MSSQLServer.Query as MSSQL
import Startup
import Util.Utilities (toSqlQuotes)

instance ReadWriteDataBase Order where
  updateEnt :: Order -> App ()
  updateEnt ent =
    let q =
          "UPDATE "
            <> entName ent
            <> " SET "
            <> "CustomerID = "
            <> show (oCId ent)
            <> ", OrderNumber = "
            <> toSqlQuotes (oNumber ent)
     in do
          config <- ask
          let res = MSSQL.sql (connectionString config) $ Text.pack (q)
          liftIO res
