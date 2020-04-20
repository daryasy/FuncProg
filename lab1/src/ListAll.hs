{-# LANGUAGE OverloadedStrings #-}

module ListAll where

import           Database.MySQL.Base
import qualified System.IO.Streams   as Streams
import           Utils

-- Provides definitions of overloaded 'ListAll' operation
class ListAll a where
  listAll :: a -> MySQLConn -> IO ([ColumnDef], Streams.InputStream [MySQLValue])

-- Type 'TableName' is instance of class 'ListAll', and here is definition of method corresponding to 'ListAll'
-- Execute SQL query "SELECT *", which shows all rows using database connection for every table
instance ListAll TableName where
  listAll Teachers conn     = query_ conn "SELECT * FROM teachers"
  listAll Sections conn     = query_ conn "SELECT * FROM sections"
  listAll Students conn     = query_ conn "SELECT * FROM students"
  listAll Schedule conn     = query_ conn "SELECT * FROM schedule"
  listAll Competitions conn = query_ conn "SELECT * FROM competitions"

-- Shows all rows of specified table
listAllManager :: TableName -> MySQLConn -> IO ()
listAllManager tableName conn = do
  (defs, is) <- listAll tableName conn
  print ("id" : tableColumns tableName)
  mapM_ print =<< Streams.toList is
