{-# LANGUAGE OverloadedStrings #-}

module FindById where

import           Database.MySQL.Base
import qualified System.IO.Streams   as Streams
import           Utils

-- Provides definitions of overloaded 'findById' operation
class FindById a where
  findById :: a -> String -> MySQLConn -> IO ([ColumnDef], Streams.InputStream [MySQLValue])

-- Type 'TableName' is instance of class 'FindById', and here is definition of method corresponding to 'findById'
-- Execute SQL query "SELECT", which finds row with specified id using database connection for every table
instance FindById TableName where
  findById Teachers index conn = query conn "SELECT * FROM teachers WHERE id=?" [MySQLInt32 (toNum index)]
  findById Sections index conn = query conn "SELECT * FROM sections WHERE id=?" [MySQLInt32 (toNum index)]
  findById Students index conn = query conn "SELECT * FROM students WHERE id=?" [MySQLInt32 (toNum index)]
  findById Schedule index conn = query conn "SELECT * FROM schedule WHERE id=?" [MySQLInt32 (toNum index)]
  findById Competitions index conn = query conn "SELECT * FROM competitions WHERE id=?" [MySQLInt32 (toNum index)]

-- Collects id from user input and finds row based on it in specified table
findByManager :: TableName -> MySQLConn -> IO ()
findByManager tableName conn = do
  putStrLn "Enter id: "
  index <- getLine
  (defs, is) <- findById tableName index conn
  print ("id" : tableColumns tableName)
  print =<< Streams.toList is
