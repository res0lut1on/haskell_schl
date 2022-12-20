{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Controllers.Order () where

import Data.Models (OrderModel (OrderModel))
import Network.HTTP.Types
import Network.Wai (Response, responseFile)
import Services.GService (GenericService (getList))
import Data.Entities
import qualified Services.GService as S
import Startup (run)

index :: Response
index =
  do
    let app = S.getList @Order @OrderModel
    res <- run app
    generateOrderPage res
    return $
      responseFile
        status200
        [("Content-Type", "text/html")]
        "src/Pages/Order/OrderMainPage.html"
        Nothing