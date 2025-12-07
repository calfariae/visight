{-# LANGUAGE OverloadedStrings #-}
module Database.Connection where

import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig, Config(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Setting as ConnectionSetting
import qualified Hasql.Connection.Setting.Connection as ConnectionSettingConnection

-- | Load environment variables from .env file at default location
loadEnv :: IO ()
loadEnv = loadFile defaultConfig

-- | Load environment variables from .env file at specified path
loadEnvFrom :: FilePath -> IO ()
loadEnvFrom path = loadFile defaultConfig { configPath = [path] }

-- | Get database connection string from environment variables
getConnectionString :: IO Text
getConnectionString = do
  host <- lookupEnv "PGHOST"
  dbname <- lookupEnv "PGDATABASE"
  user <- lookupEnv "PGUSER"
  password <- lookupEnv "PGPASSWORD"
  port <- lookupEnv "PGPORT"
  
  -- | Something to fallback to in case if there's no .env
  let host' = fromMaybe "localhost" host
      dbname' = fromMaybe "mydatabase" dbname
      user' = fromMaybe "myuser" user
      password' = fromMaybe "mypassword" password
      port' = fromMaybe "5432" port
  
  return $ T.pack $ "host=" ++ host' ++ 
                    " dbname=" ++ dbname' ++ 
                    " user=" ++ user' ++ 
                    " password=" ++ password' ++ 
                    " port=" ++ port'

-- | Database connection settings
connectionSettings :: Text -> [ConnectionSetting.Setting]
connectionSettings connStr = 
  [ConnectionSetting.connection $ ConnectionSettingConnection.string connStr]

-- | Acquire a database connection
acquireConnection :: IO (Either Connection.ConnectionError Connection.Connection)
acquireConnection = do
  connStr <- getConnectionString
  Connection.acquire (connectionSettings connStr)

-- | Acquire a database connection with explicit .env loading from default location
acquireConnectionWithEnv :: IO (Either Connection.ConnectionError Connection.Connection)
acquireConnectionWithEnv = do
  loadEnv  -- Load .env file from default location
  acquireConnection

-- | Acquire a database connection with explicit .env loading from custom path
acquireConnectionWithEnvFrom :: FilePath -> IO (Either Connection.ConnectionError Connection.Connection)
acquireConnectionWithEnvFrom envPath = do
  loadEnvFrom envPath  -- Load .env file from specified path
  acquireConnection