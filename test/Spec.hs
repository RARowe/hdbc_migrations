import qualified Data.Either as E
import Lib (databaseNeedsUpdating)
import Models

main :: IO ()
main = runTests

testMigrationConfig :: MigrationConfig
testMigrationConfig = MigrationConfig { databaseVersion = 1
                                      , desiredVersion = length testMigrations
                                      , migrations = testMigrations }

testMigrations :: [Migration]
testMigrations = [ Migration { upSql = "create table Users (FirstName string not null);", downSql = "" }
                 , Migration { upSql = "insert into Users values ('Andrew');", downSql = "" }
                 , Migration { upSql = "create table Pets (Name string not null);", downSql = "" } ]

putStrLnSuccess :: String -> IO ()
putStrLnSuccess s = putStrLn $ "\x1b[32m" ++ s ++ "\x1b[0m"

putStrLnFailure :: String -> IO ()
putStrLnFailure s = putStrLn $ "\x1b[31m" ++ s ++ "\x1b[0m"

runTests :: IO ()
runTests = do
  putStrLn "Tests running..."
  let (failures, successes) = E.partitionEithers tests
  if length successes > 0 then putStrLnSuccess $ (show $ length successes) ++ " tests passed!" else return ()
  if length failures > 0 then putStrLnFailure $ unlines failures else return ()
  putStrLn $ (show $ length successes) ++ " out of " ++ (show $ length tests) ++ " passed."

tests :: [Either String Bool]
tests = [databaseNeedsUpdatingTest, databaseNeedsUpdatingTest2]

databaseNeedsUpdatingTest :: Either String Bool
databaseNeedsUpdatingTest
  | databaseNeedsUpdating testMigrationConfig == True = Right True
  | otherwise = Left "databaseNeedsUpdating: Should return 'True' when database version is behind desired version"

databaseNeedsUpdatingTest2 :: Either String Bool
databaseNeedsUpdatingTest2
  | databaseNeedsUpdating testMigrationConfig{ databaseVersion = 234 } == False = Right True
  | otherwise = Left "databaseNeedsUpdating: Should return 'False' when database version is ahead of desired version"
