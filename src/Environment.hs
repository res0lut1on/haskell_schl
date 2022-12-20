module Environment (filePath, pageSize, defaultConnectInfo, avaibleport) where

import Database.MSSQLServer.Connection (ConnectInfo (..))

filePath :: FilePath
filePath = "/home/pineapple/doc/project_cabal/src/Data/ContextTxt"

pageSize :: Int
pageSize = 4

avaibleport :: Int
avaibleport = 3005

defaultConnectInfo :: ConnectInfo
defaultConnectInfo =
  defaultConnectInfo
    { connectHost = "192.168.0.1",
      connectPort = "1433",
      connectDatabase = "HaskellDatabase",
      connectUser = "dbo",
      connectPassword = "some_password"
    }
