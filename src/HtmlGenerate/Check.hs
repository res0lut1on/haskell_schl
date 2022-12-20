{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerate.Check (mainPage, orderPage) where

import Control.Monad.RWS
import Data.Entities (Order)
import Database.MSSQLServer.Connection (ConnectInfo (..), connect)
import Database.MSSQLServer.Query
import qualified Database.MSSQLServer.Query as MSSQL
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

mainPage :: Html
mainPage = docTypeHtml $ do
  H.head $ do
    H.title "Main Page"
  body $ do
    a "A list of orders:" ! href "/Orders"
    a "A list of products:" ! href "/Products"

orderPage :: (ToMarkup a) => [[a]] -> Html
orderPage xs =
  docTypeHtml $ do
    H.head $ do
      H.title "Order Page"
    body $ do
      table ! class_ "table table-striped table-hover" $
        thead $
          forM_ xs (tr . mapM_ (td . toHtml))

selectOrder conn = return $ docTypeHtml $ do
  H.head $ do
    H.title "Order Page"
  body $ do
    p conn

rend :: IO ()
rend =
  let defaultConnectInfo =
        defaultConnectInfo
          { connectHost = "192.168.0.1",
            connectPort = "1433",
            connectDatabase = "HaskellDatabase",
            connectUser = "dbo",
            connectPassword = "some_password"
          }
   in do
        conn <- connect defaultConnectInfo
        [Only num] <- MSSQL.sql conn "SELECT 111" :: IO [Only Int]
        writeFile ("/home/pineapple/doc/project_cabal/src/Pages/MainPage.html") $ renderHtml $ orderPage [[num]]

instance ToMarkup Order