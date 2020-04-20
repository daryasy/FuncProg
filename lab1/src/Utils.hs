module Utils where

import           Data.Int
import           Data.Time

-- Define new data type, which can have one of listed values
data TableName
  = Teachers
  | Sections
  | Students
  | Schedule
  | Competitions

-- List of existing tables
tableNames :: [String]
tableNames = ["teachers", "sections", "students", "schedule", "competitions"]

-- Converts numeric symbols to integer
toNum :: String -> Int32
toNum str = fromInteger (read str :: Integer)

-- Converts time string to time format
toTime :: String -> TimeOfDay
toTime dateStr = parseTimeOrError True defaultTimeLocale "%H:%M" dateStr :: TimeOfDay

-- Converts date string to date format
toDate :: String -> LocalTime
toDate dateStr = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M" dateStr :: LocalTime

-- Check if name of table is valid
checkTableName :: String -> Bool
checkTableName name = name `elem` tableNames

-- Lists of fields, which should be filled by user, when row is created (for each table)
tableColumns :: TableName -> [String]
tableColumns Teachers     = ["name", "surname"]
tableColumns Sections     = ["name", "teacherID"]
tableColumns Students     = ["name", "surname", "sectionID"]
tableColumns Schedule     = ["sectionID", "beginDay", "beginTime", "endTime"]
tableColumns Competitions = ["sectionID", "beginTime", "endTime"]

-- Lists of fields, which can be modified by user (for each table)
updatableTableColumns :: String -> [String]
updatableTableColumns "teachers" = ["name", "surname"]
updatableTableColumns "sections" = ["name", "teacherID"]
updatableTableColumns "students" = ["name", "surname", "sectionID"]
updatableTableColumns "schedule" = ["sectionID", "beginDay", "beginTime", "endTime"]
updatableTableColumns "competitions" = ["sectionID", "beginTime", "endTime"]
updatableTableColumns x = []

-- Checks if specified field can be modified by user
checkUpdatableColumns :: String -> String -> Bool
checkUpdatableColumns tableName columnName = columnName `elem` updatableTableColumns tableName

-- Converts string to TableName data type
getTableName :: String -> TableName
getTableName "teachers"     = Teachers
getTableName "sections"     = Sections
getTableName "students"     = Students
getTableName "schedule"     = Schedule
getTableName "competitions" = Competitions
