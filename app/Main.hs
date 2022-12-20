module Main (main) where

import Control.Exception
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Internal as BS
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  let port = 3000
  putStrLn $ "starting on port " ++ show port
  run port app

app :: Application
app req respond =
  bracket_
    (putStrLn "Allocating scarce resource")
    (putStrLn "Cleaning up")
    (respond $ responseLBS status200 [] $ BS.packChars "Hello World")
