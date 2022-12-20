module Environment (filePath, pageSize, defaultConnectInfo, avaibleport) where

filePath :: FilePath
filePath = "/home/pineapple/doc/project_cabal/src/Data/ContextTxt"

pageSize :: Int
pageSize = 4

avaibleport :: Int
avaibleport = 3005

defaultConnectInfo :: DefaultConnectInfo
defaultConnectInfo =
  DefaultConnectInfo
    { connectHost = "192.168.0.1",
      connectPort = "1433",
      connectDatabase = "HaskellDatabase",
      connectUser = "dbo",
      connectPassword = "some_password"
    }

data DefaultConnectInfo = DefaultConnectInfo
  { connectHost :: String,
    connectPort :: String,
    connectDatabase :: String,
    connectUser :: String,
    connectPassword :: String
  }