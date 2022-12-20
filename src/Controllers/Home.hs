{-# LANGUAGE OverloadedStrings #-}

module Controllers.Home (index) where

import Network.HTTP.Types
import Network.Wai (Response, responseFile)

index :: Response
index =
  responseFile
    status200
    [("Content-Type", "text/html")]
    "src/Pages/MainPage.html"
    Nothing
