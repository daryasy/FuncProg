{-# LANGUAGE OverloadedStrings #-}

module Update where

import           Data.List
import           Data.Text.Conversions
import           Database.MySQL.Base
import           Utils

-- Provides definitions of overloaded 'updateRow' operation
class Update a where
  updateRow :: a -> String -> String -> String -> MySQLConn -> IO OK

-- Type 'TableName' is instance of class 'Update', and here is definition of method corresponding to 'updateRow'
-- Execute SQL query "UPDATE", which modifies the existing records with specified value and database connection for every table
instance Update TableName where
  updateRow Teachers "name" value index conn =
    execute conn "UPDATE teachers SET name=? WHERE id=?" [MySQLText (toText value), MySQLInt32 (toNum index)]
  updateRow Teachers "surname" value index conn =
    execute conn "UPDATE teachers SET surname=? WHERE id=?" [MySQLText (toText value), MySQLInt32 (toNum index)]
  updateRow Sections "name" value index conn =
    execute conn "UPDATE sections SET name=? WHERE id=?" [MySQLText (toText value), MySQLInt32 (toNum index)]
  updateRow Sections "teacherID" value index conn =
    execute conn "UPDATE sections SET teacherID=? WHERE id=?" [MySQLInt32 (toNum value), MySQLInt32 (toNum index)]
  updateRow Students "name" value index conn =
    execute conn "UPDATE students SET name=? WHERE id=?" [MySQLText (toText value), MySQLInt32 (toNum index)]
  updateRow Students "surname" value index conn =
    execute conn "UPDATE students SET surname=? WHERE id=?" [MySQLText (toText value), MySQLInt32 (toNum index)]
  updateRow Students "sectionID" value index conn =
    execute conn "UPDATE students SET sectionID=? WHERE id=?" [MySQLInt32 (toNum value), MySQLInt32 (toNum index)]
  updateRow Schedule "sectionID" value index conn =
    execute conn "UPDATE schedule SET sectionID=? WHERE id=?" [MySQLInt32 (toNum value), MySQLInt32 (toNum index)]
  updateRow Schedule "beginDay" value index conn =
    execute conn "UPDATE schedule SET beginDay=? WHERE id=?" [MySQLText (toText value), MySQLInt32 (toNum index)]
  updateRow Schedule "beginTime" value index conn =
    execute conn "UPDATE schedule SET beginTime=? WHERE id=?" [MySQLTime 0 (toTime value), MySQLInt32 (toNum index)]
  updateRow Schedule "endTime" value index conn =
    execute conn "UPDATE schedule SET endTime=? WHERE id=?" [MySQLTime 0 (toTime value), MySQLInt32 (toNum index)]
  updateRow Competitions "sectionID" value index conn =
    execute conn "UPDATE competitions SET sectionID=? WHERE id=?" [MySQLInt32 (toNum value), MySQLInt32 (toNum index)]
  updateRow Competitions "beginTime" value index conn =
    execute
      conn
      "UPDATE competitions SET beginTime=? WHERE id=?"
      [MySQLDateTime (toDate value), MySQLInt32 (toNum index)]
  updateRow Competitions "endTime" value index conn =
    execute conn "UPDATE competitions SET endTime=? WHERE id=?" [MySQLDateTime (toDate value), MySQLInt32 (toNum index)]

-- Collects id, field and new value from user input and updates row based on them
updateRowManager :: String -> MySQLConn -> IO ()
updateRowManager name conn = do
  putStrLn "Enter row id: "
  index <- getLine
  putStrLn "Choose field you want to update from the list: "
  putStrLn (intercalate "\n" (updatableTableColumns name))
  field <- getLine
  if checkUpdatableColumns name field
    then do
      putStrLn "Enter new value: "
      value <- getLine
      updateRow (getTableName name) field value index conn
      putStrLn "Updated !!!"
    else putStrLn "Wrong input"
