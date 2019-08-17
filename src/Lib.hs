module Lib
    ( runMigrations
    ) where
import DataSource
import Models
import Data.List as L (intercalate)
import Data.Maybe as M (fromMaybe)
import Database.HDBC

runMigrations :: IConnection conn => conn -> [Migration] -> IO () 
runMigrations conn ms = do
  migrationVersionTableExists <- retrieveSingleBool conn migrationVersionTableExistsSql
  case migrationVersionTableExists of
    Just True -> do
      dbVersion <- retrieveSingleInt conn databaseMigrationVersionSql
      runMig MigrationConfig { databaseVersion = M.fromMaybe 0 dbVersion
                             , desiredVersion = desVersion
                             , migrations = ms }
    Just False -> runMig MigrationConfig { databaseVersion = 0
                                         , desiredVersion = desVersion
                                         , migrations = freshMigrations }
    Nothing -> return ()
  where runMig = runMigrationsWithConfig conn
        desVersion = length ms
        freshMigrations = createMigrationVersionTable : createMigrationVersionEntry : ms

runMigrationsWithConfig :: IConnection conn => conn -> MigrationConfig -> IO ()
runMigrationsWithConfig conn config
  | databaseNeedsUpdating config = executeAndCommitSql conn $ updateDatabaseSql config
  | otherwise = return ()

createMigrationVersionTable :: Migration
createMigrationVersionTable = Migration { upSql = createMigrationVersionTableSql, downSql = "" }

createMigrationVersionEntry :: Migration
createMigrationVersionEntry = Migration { upSql = createMigrationVersionEntrySql, downSql = "" }

createMigrationVersionTableSql :: String
createMigrationVersionTableSql = "create table MigrationVersion (Version int not null);";

createMigrationVersionEntrySql :: String
createMigrationVersionEntrySql = "insert into MigrationVersion values (0);"

migrationVersionTableExistsSql :: String
migrationVersionTableExistsSql = "select count(1) from sqlite_master where type='table' and name='MigrationVersion';"

databaseMigrationVersionSql :: String
databaseMigrationVersionSql = "select Version from MigrationVersion limit 1;";

updateDatabaseSql :: MigrationConfig -> String
updateDatabaseSql config = L.intercalate ";" $ map upSql $ migrationsToRun config

databaseNeedsUpdating :: MigrationConfig -> Bool
databaseNeedsUpdating config = desiredVersion config > databaseVersion config

migrationsToRun :: MigrationConfig -> [Migration]
migrationsToRun mc = drop (databaseVersion mc) (migrations mc)
