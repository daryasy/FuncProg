{-# LANGUAGE OverloadedStrings #-}

module Delete where

import           Database.MySQL.Base
import           Utils

-- Provides definitions of overloaded 'deleteRow' operation
class Delete a where
  deleteRow :: a -> String -> MySQLConn -> IO OK

-- Type 'TableName' is instance of class 'Delete', and here is definition of method corresponding to 'deleteRow'
-- Execute SQL query "DELETE", which delete row with specified id using database connection for every table
instance Delete TableName where
  deleteRow Teachers index conn = execute conn "DELETE FROM teachers WHERE id=?" [MySQLInt32 (toNum index)]
  deleteRow Sections index conn = execute conn "DELETE FROM sections WHERE id=?" [MySQLInt32 (toNum index)]
  deleteRow Students index conn = execute conn "DELETE FROM students WHERE id=?" [MySQLInt32 (toNum index)]
  deleteRow Schedule index conn = execute conn "DELETE FROM schedule WHERE id=?" [MySQLInt32 (toNum index)]
  deleteRow Competitions index conn = execute conn "DELETE FROM competitions WHERE id=?" [MySQLInt32 (toNum index)]

-- Collects id from user input and deletes row based on it
deleteRowManager :: TableName -> MySQLConn -> IO ()
deleteRowManager tableName conn = do
  putStrLn "Enter id: "
  index <- getLine
  deleteRow tableName index conn
  putStrLn "Deleted !!!"
