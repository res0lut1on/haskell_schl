{-# LANGUAGE OverloadedStrings #-}

module Spec
  (
  )
where

import Control.Exception
import Control.Monad.Trans
import Controllers.Home as Home
import Controllers.Order as OrderContr
import qualified Data.ByteString.Lazy.Internal as BS
import Data.Text
import qualified Data.Text as T
import Environment ()
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Util.Utilities (mainRoute)

main :: IO ()
main = do
  let port = avaibleport
  putStrLn $ "starting on port " ++ show port
  putStrLn $ "http://localhost:" ++ show port ++ "/"
  run port app

app :: Application
app req respond = case mainRoute (pathInfo req) of
  "Orders" -> OrderContr.orderController req (pathInfo req) >>= \res -> respond res
  _ -> respond Home.index
