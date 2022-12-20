{-# LANGUAGE OverloadedStrings #-}

module Program (maindo, mainIndex) where

-- import Data.Time.Clock
-- import Data.Time.Format

import Control.Exception
import Control.Monad.Trans ()
import qualified Data.ByteString.Lazy.Internal as BS
import Data.Text
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

maindo = do
  let port = 3000
  putStrLn $ "starting on port " ++ show port
  run port app

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

handleRoute :: [Text] -> Text
handleRoute (x : _) = x
handleRoute [] = T.pack ""

app :: Application
app req respond = case handleRoute (pathInfo req) of
  "" -> respond mainIndex
  "MainPage" -> respond $ responseLBS status200 [("Content-Type", "text/plain")] $ BS.packChars ""

mainIndex :: Response
mainIndex =
  responseFile
    status200
    [("Content-Type", "text/html")]
    "src/Pages/MainPage.html"
    Nothing