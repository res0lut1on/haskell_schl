module Main (main) where

import LibFold
import Startup (App)
import Network.Socket

main :: IO ()
main = return ()
-- main = withSocketsDo $ handleSqlError $
--     do args <- getArgs
--        dbh <- connect "pod.db"
--        case args of
--          ["add", url] -> add dbh url
--          ["update"] -> update dbh
--          ["download"] -> download dbh
--          ["fetch"] -> do update dbh
--                          download dbh
--          _ -> syntaxError
--        disconnect dbh


