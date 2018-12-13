{-# LANGUAGE FlexibleContexts #-}

module DataSource2 where

import           Control.Monad.IO.Class
import           Database.HDBC                   (commit, disconnect)
import           Database.HDBC.PostgreSQL        (Connection, connectPostgreSQL)
import           Database.HDBC.Query.TH          (defineTableFromDB)
import           Database.HDBC.Record
import           Database.HDBC.Schema.Driver     (typeMap)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Language.Haskell.TH             (Dec, Name, Q, TypeQ)

-- createPool' config = createPool (connect config) disconnect (dbPoolStripeNum config) (realToFrac (dbPoolKeepTime config)) (dbPoolResourceNum config)

connect :: IO Connection
connect =
  connectPostgreSQL $ concat [
  "user='", "admin", "'"
  , "password='", "admin", "'"
  , "dbname='", "admin", "'"
  , "host='", "localhost", "'"
  , "port='", "15432", "'"
  ]

-- connectWithLoadConfig :: IO Connection
-- connectWithLoadConfig = do
--   c <- loadConfig
--   connect c

defineTable :: String -> [Name] -> Q [Dec]
defineTable =
  defineTableFromDB
    connect
    (driverPostgreSQL { typeMap = convTypes })
    "public"

convTypes :: [(String, TypeQ)]
convTypes = []


runQuery a b = liftIO $ do
  conn <- liftIO connect
  runQuery' conn a b
insertM a b = liftIO connect >>= \conn -> liftIO $ runInsert conn a b >> commit conn
deleteM a b = liftIO connect >>= \conn -> liftIO $ runDelete conn a b >> commit conn
updateM a b = liftIO connect >>= \conn -> liftIO $ runUpdate conn a b >> commit conn
