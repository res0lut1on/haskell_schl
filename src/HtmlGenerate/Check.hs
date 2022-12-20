{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module HtmlGenerate.Check (mainPage, generateOrderPage) where

import Control.Monad.RWS
import Data.Entities (Order (oNumber))
import Data.Models (OrderModel (orderModelNumber))
import Database.MSSQLServer.Connection (ConnectInfo (..), connect)
import Database.MSSQLServer.Query
import qualified Database.MSSQLServer.Query as MSSQL
import Environment (avaibleport)
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

generateOrderPage :: [OrderModel] -> Html
generateOrderPage xs =
  docTypeHtml $ do
    H.head $ do
      H.title "Order Page"
    body $ do
      table ! class_ "table table-striped table-hover" $
        do
          tr $ do
            th "Number Order"
            th "Refer to order details"
          forM_
            xs
            ( \ord -> do
                tr $ do
                  td $ toHtml $ orderModelNumber ord
                  td $ toHtml ("http://localhost:" ++ show avaibleport ++ "/" ++ orderModelNumber ord)
            )


