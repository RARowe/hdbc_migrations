module IntegrationTests
  ( runIntegrationTests
  ) where
import CommonTest (putStrLnSuccess, putStrLnFailure)
import Lib (runMigrations)
import Models
import Database.HDBC.Sqlite3 (connectSqlite3)
import DataSource (retrieveSingleBool)
import Data.Maybe (fromMaybe)
import MigrationDataAccess (retrieveMigrationVersion)

runIntegrationTests :: IO ()
runIntegrationTests = do
  putStrLn "Integration test(s) running..."
  setup
  runTest

setup :: IO ()
setup = writeFile "itest_blank.db" ""

runTest :: IO ()
runTest = do
  conn <- connectSqlite3 "itest_blank.db"
  runMigrations conn testMigrations
  let getBool = retrieveSingleBool conn
  userTableExists <- getBool "select count(1) from sqlite_master where type='table' and name='Users';"
  userExists <- getBool "select count(1) from Users where FirstName = 'Andrew'";
  petTableExists <- getBool "select count(1) from sqlite_master where type='table' and name='Pets';"
  migrationVersion <- retrieveMigrationVersion conn
  case foldl reduceMaybeBool True [userTableExists, userExists, petTableExists, fmap ((==) 3) migrationVersion] of
    True -> putStrLnSuccess "'runMigrations' works as expected!"
    False -> putStrLnFailure "'runMigrations' does not work as expected!"

testMigrations :: [Migration]
testMigrations = [ Migration { upSql = "create table Users (FirstName string not null);", downSql = "" }
                 , Migration { upSql = "insert into Users values ('Andrew');", downSql = "" }
                 , Migration { upSql = "create table Pets (Name string not null);", downSql = "" } ]

reduceMaybeBool :: Bool -> Maybe Bool -> Bool
reduceMaybeBool _ Nothing = False
reduceMaybeBool False _ = False
reduceMaybeBool x (Just y) = x && y
