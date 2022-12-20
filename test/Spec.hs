{-# LANGUAGE OverloadedStrings #-}

module Spec
  (
  )
where

import Control.Exception
import Control.Monad.Trans
import Controllers.Home as Home
import qualified Data.ByteString.Lazy.Internal as BS
import Data.Text
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Util.Utilities (mainRoute)
import Environment ()

main :: IO ()
main = do
  let port = avaibleport
  putStrLn $ "starting on port " ++ show port
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  run port app

app :: Application
app req respond = case mainRoute (pathInfo req) of
  "" -> respond Home.index
  "MainPage" -> respond $ responseLBS status200 [("Content-Type", "text/plain")] $ BS.packChars ""
  "Orders" -> respond
