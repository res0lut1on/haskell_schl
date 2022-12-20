{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Data.DataBase.InitDataBase () where

import Database.MSSQLServer.Connection
import Database.MSSQLServer.Query
import qualified Database.MSSQLServer.Query as MSSQL 

mai :: IO ()
mai =
  let info =
        defaultConnectInfo
          { connectHost = "192.168.0.1",
            connectPort = "1433",
            connectDatabase = "HaskellDatabase",
            connectUser = "dbo",
            connectPassword = "some_password"
          }
   in do
        conn <- connect info
        rs <- MSSQL.sql conn "SELECT 2 + 2" :: IO [Only Int]
        print rs

connectDB :: ConnectInfo -> IO Connection
connectDB fp =
    do dbh <- connect fp
       prepDB dbh
       return dbh

select_exp1 :: Connection -> IO ()
select_exp1 conn = do
  [num] <- MSSQL.sql conn "SELECT 111" :: IO [Only Int]
  print num

prepDB :: Connection -> IO ()
prepDB dbh =
  do
    tables <- getTables dbh
    unless ("Customer" `elem` tables) $
      do
        run
          dbh
          "CREATE TABLE Customer (\
          \CustomerID INT NOT NULL IDENTITY,\
          \CustomerName NVARCHAR(30) NOT NULL,\
          \CustomerAddress NVARCHAR(30) NOT NULL,\
          \PRIMARY KEY (CustomerID))"
          []
        return ()
    unless ("Shop" `elem` tables) $
      do
        run
          dbh
          "ShopID INT NOT NULL IDENTITY (\
          \ShopName NVARCHAR(30) NOT NULL,\
          \ShopAddress NVARCHAR(30) NOT NULL,\
          \PRIMARY KEY (ShopID))"
          []
        return ()
    unless ("ProductColor" `elem` tables) $
      do
        run
          dbh
          "CREATE TABLE ProductColor (\
          \ColorID INT NOT NULL IDENTITY,\
          \ColorName NVARCHAR(30) NOT NULL,\
          \PRIMARY KEY (ColorID))"
          []
        return ()
    unless ("Product" `elem` tables) $
      do
        run
          dbh
          "ProductID INT NOT NULL IDENTITY (\
          \ShopName NVARCHAR(30) NOT NULL,\
          \ShopID INT,\
          \ProductName NVARCHAR(30) NOT NULL,\
          \Price REAL NOT NULL,\
          \ColorID INT,\
          \CONSTRAINT FK_Shop_ShopID\
            \FOREIGN KEY (ShopID) \
            \REFERENCES Shop (ShopID),\
          \CONSTRAINT FK_Shop_ColorID\
            \FOREIGN KEY (ColorID)\
            \REFERENCES ProductColor (ColorID)\
            \ON DELETE SET NULL,\
          \PRIMARY KEY (ProductID))"
          []
        return ()
    unless ("Ord" `elem` tables) $
      do
        run
          dbh
          "CREATE TABLE Ord (\
          \OrderID INT NOT NULL IDENTITY,\
          \CustomerID INT,\
          \OrderNumber NVARCHAR(30) NOT NULL,\
          \CONSTRAINT FK_Users_UserData\
            \FOREIGN KEY (CustomerID) \
            \REFERENCES Customer (CustomerID) \
            \ON DELETE SET NULL,\            
          \PRIMARY KEY (OrderID))"
          []
        return ()
    unless ("ProductOrder" `elem` tables) $
      do
        run
          dbh
          "CREATE TABLE ProductOrder (\
          \ProductOrderID INT NOT NULL IDENTITY,\
          \ProductID INT,\
          \OrderID INT,\
          \CONSTRAINT FK_ProductOrder_ProductID\
            \FOREIGN KEY (ProductID)\
            \REFERENCES Product (ProductID)\
            \ON DELETE SET NULL,\
          \CONSTRAINT FK_ProductOrder_OrderID\
          \FOREIGN KEY (OrderID) \
            \REFERENCES Ord (OrderID) \
            \ON DELETE SET NULL,\
          \PRIMARY KEY (ProductOrderID))"
          []
        return ()
    commit dbh

