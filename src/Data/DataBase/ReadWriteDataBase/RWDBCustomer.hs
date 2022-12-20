{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Data.DataBase.ReadWriteDataBase.RWDBCustomer () where

import Control.Monad.RWS
import Data.DataBase.ReadWriteDataBase.ReadWriteDataBaseClass
import Data.Entities (Customer (customerAddress, customerName))
import Data.RepEntity.BaseEntity
import Data.Text as Text
import qualified Database.MSSQLServer.Query as MSSQL
import Startup
import Util.Utilities (toSqlQuotes)

instance ReadWriteDataBase Customer where
  addNewEnt :: Customer -> App ()
  addNewEnt cust = do
    config <- ask
    liftIO $
      MSSQL.sql
        (connectionString config)
        ("")

  updateEnt :: Customer -> App ()
  updateEnt ent =
    let q = "UPDATE " <> entName ent <> " SET " <> "CustomerName = " <> toSqlQuotes (customerName ent) <> ", CustomerAddress = " <> toSqlQuotes (customerAddress ent)
     in do
          config <- ask
          let res = MSSQL.sql (connectionString config) $ Text.pack (q)
          liftIO res

-- addPodcast :: conn -> a -> App ()
-- addPodcast dbh podcast =
--   handleSql errorHandler $
--     do
--       -- Insert the castURL into the table.  The database
--       -- will automatically assign a cast ID.
--       run
--         dbh
--         "INSERT INTO podcasts (castURL) VALUES (?)"
--         [toSql (castURL podcast)]
--       -- Find out the castID for the URL we just added.
--       r <-
--         quickQuery'
--           dbh
--           "SELECT castid FROM podcasts WHERE castURL = ?"
--           [toSql (castURL podcast)]
--       case r of
--         [[x]] -> return $ podcast {castId = fromSql x}
--         y -> fail $ "addPodcast: unexpected result: " ++ show y
--   where
--     errorHandler e =
--       do
--         fail $
--           "Error adding podcast; does this URL already exist?\n"
--             ++ show e

-- -- | Adds a new episode to the database.
-- --
-- -- Since this is done by automation, instead of by user request, we will
-- -- simply ignore requests to add duplicate episodes.  This way, when we are
-- -- processing a feed, each URL encountered can be fed to this function,
-- -- without having to first look it up in the DB.
-- --
-- -- Also, we generally won't care about the new ID here, so don't bother
-- -- fetching it.

-- -- | Modifies an existing podcast.  Looks up the given podcast by
-- -- ID and modifies the database record to match the passed Podcast.
-- updatePodcast :: IConnection conn => conn -> Podcast -> IO ()
-- updatePodcast dbh podcast =
--   run
--     dbh
--     "UPDATE podcasts SET castURL = ? WHERE castId = ?"
--     [toSql (castURL podcast), toSql (castId podcast)]
--     >> return ()
