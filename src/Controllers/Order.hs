{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Controllers.Order (orderController) where

import qualified Data.ByteString.Lazy.Internal as BS
import Data.Entities
import Data.Models (OrderModel (OrderModel))
import Data.Text
import HtmlGenerate.Check (generateOrderPage)
import Network.HTTP.Types
import Network.Wai (Request, Response, responseFile, responseLBS)
import Services.GService (GenericService (getList))
import qualified Services.GService as S
import Startup (AppData (appResult), AppResult (..), run)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Util.Utilities

orderController :: Request -> [Text] -> IO Response
orderController req (x : path) =
  case mainRoute path of
    "" -> orderIndex >>= \res -> return $ responseLBS status200 [("Content-Type", "text/html")] $ BS.packChars $ renderHtml $ res

orderIndex :: IO Html
orderIndex =
  do
    let app = S.getList @Order @OrderModel
    res <- Startup.run app
    return $ generateOrderPage (appResult $ result res)
