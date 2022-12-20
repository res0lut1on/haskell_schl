{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.DataBase.ReadWriteDataBase.RWDBProductOrder () where

import Control.Monad.RWS
import Data.DataBase.ReadWriteDataBase.ReadWriteDataBaseClass
  ( ReadWriteDataBase (updateEnt),
  )
import Data.Entities (ProductOrder (opId, poId))
import Data.RepEntity.BaseEntity
import Data.Text as Text
import qualified Database.MSSQLServer.Query as MSSQL
import Startup

instance ReadWriteDataBase ProductOrder where
  updateEnt :: ProductOrder -> App ()
  updateEnt ent =
    let q =
          "UPDATE "
            <> entName ent
            <> " SET "
            <> "ProductID = "
            <> show (poId ent)
            <> ", OrderID = "
            <> show (opId ent)
     in do
          config <- ask
          let res = MSSQL.sql (connectionString config) $ Text.pack (q)
          liftIO res
