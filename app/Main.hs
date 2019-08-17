module Main where

import Database.HDBC as DB (disconnect)
import Database.HDBC.Sqlite3 as SL (connectSqlite3)
import Lib
import Models


main :: IO ()
main = do
  conn <- SL.connectSqlite3 "test.db"
  runMigrations conn myMigrations
  DB.disconnect conn

myMigrations :: [Migration]
myMigrations = [Migration { upSql = "create table if not exists Users (FirstName text not null);", downSql = "drop table Users;" }
                ,Migration { upSql = "alter table Users add new column Age int default 0 not null;", downSql = "" }
                ,Migration { upSql = "create table if not exists Pets (Name text not null);", downSql = "drop table Pets;" }]
