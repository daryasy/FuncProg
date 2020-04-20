{-# LANGUAGE OverloadedStrings #-}

module Create where

import           Data.List
import           Data.Text.Conversions
import           Database.MySQL.Base
import           Utils

-- Provides definitions of overloaded 'createRow' operation
class Create a where
  createRow :: a -> [String] -> MySQLConn -> IO OK

-- Type 'TableName' is instance of class 'Create', and here is definition of method corresponding to 'createRow'
-- Execute SQL query "INSERT" with specified values of new row and database connection for every table
instance Create TableName where
  createRow Teachers params conn =
    execute
      conn
      "INSERT INTO teachers (name,surname) VALUES(?,?)"
      [MySQLText (toText (head params)), MySQLText (toText (params !! 1))]
  createRow Sections params conn =
    execute
      conn
      "INSERT INTO sections (name,teacherID) VALUES(?,?)"
      [MySQLText (toText (head params)), MySQLInt32 (toNum (params !! 1))]
  createRow Students params conn =
    execute
      conn
      "INSERT INTO students (name,surname,sectionID) VALUES(?,?,?)"
      [MySQLText (toText (head params)), MySQLText (toText (params !! 1)), MySQLInt32 (toNum (params !! 2))]
  createRow Schedule params conn =
    execute
      conn
      "INSERT INTO schedule (sectionID,beginDay,beginTime,endTime) VALUES(?,?,?,?)"
      [ MySQLInt32 (toNum (head params))
      , MySQLText (toText (params !! 1))
      , MySQLTime 0 (toTime (params !! 2))
      , MySQLTime 0 (toTime (params !! 3))
      ]
  createRow Competitions params conn =
    execute
      conn
      "INSERT INTO competitions (sectionID,beginTime,endTime) VALUES(?,?,?)"
      [MySQLInt32 (toNum (head params)), MySQLDateTime (toDate (params !! 1)), MySQLDateTime (toDate (params !! 2))]

-- Collects values from user input and creates rows based on them
createRowManager :: TableName -> MySQLConn -> IO ()
createRowManager tableName conn = do
  putStrLn "Enter these values:"
  putStrLn (intercalate "\n" (tableColumns tableName))
  case tableName of
    Teachers -> do
      field0 <- getLine
      field1 <- getLine
      createRow tableName [field0, field1] conn
    Sections -> do
      field0 <- getLine
      field1 <- getLine
      createRow tableName [field0, field1] conn
    Students -> do
      field0 <- getLine
      field1 <- getLine
      field2 <- getLine
      createRow tableName [field0, field1, field2] conn
    Schedule -> do
      field0 <- getLine
      field1 <- getLine
      field2 <- getLine
      field3 <- getLine
      createRow tableName [field0, field1, field2, field3] conn
    Competitions -> do
      field0 <- getLine
      field1 <- getLine
      field2 <- getLine
      createRow tableName [field0, field1, field2] conn
  putStrLn "Created !!!"
