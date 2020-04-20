{-# LANGUAGE OverloadedStrings #-}

import           Create
import           Data.List
import           Database.MySQL.Base
import           Delete
import           FindById
import           ListAll
import           System.Exit
import           Update
import           Utils

-- Command line interface
main = do
  conn <- connect defaultConnectInfo {ciUser = "root", ciPassword = "slonik123", ciDatabase = "faculty_sport"}
  putStrLn "\nChoose a table from the list:"
  putStrLn (intercalate "\n" tableNames)
  putStrLn "else exit\n"
  name <- getLine
  putStrLn ""
  if checkTableName name
    then do
      putStrLn
        "Choose an operation from the list:\nl - list all, f - find by id, c - create, u - update, d - delete, else go back\n"
      x <- getLine
      putStrLn ""
      case x of
        "l" -> listAllManager (getTableName name) conn
        "f" -> findByManager (getTableName name) conn
        "c" -> createRowManager (getTableName name) conn
        "u" -> updateRowManager name conn
        "d" -> deleteRowManager (getTableName name) conn
        _   -> putStrLn "Going back !!!"
    else do
      putStrLn "Finished !!!"
      close conn
      exitSuccess
  close conn
  main
