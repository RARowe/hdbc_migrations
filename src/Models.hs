module Models
  ( MigrationConfig(..)
  , Migration(..)
  ) where

data MigrationConfig = MigrationConfig { databaseVersion :: Int
                                       , desiredVersion :: Int
                                       , migrations :: [Migration] }

data Migration = Migration { upSql :: String
                           , downSql :: String }
