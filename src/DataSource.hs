module DataSource
    ( retrieveSingleInt
    , retrieveSingleBool
    , executeAndCommitSql
    ) where
import Data.List as L (find)
import Data.Maybe (Maybe)
import Database.HDBC
import Database.HDBC.Sqlite3

executeAndCommitSql :: IConnection conn => conn -> String -> IO ()
executeAndCommitSql conn sql = runRaw conn sql >> commit conn

retrieveSingleInt :: IConnection conn => conn -> String -> IO (Maybe Int)
retrieveSingleInt conn sql = retrieveSingleValue conn sql >>= \i -> return $ fmap fromSql i

retrieveSingleBool :: IConnection conn => conn -> String -> IO (Maybe Bool)
retrieveSingleBool conn sql = retrieveSingleValue conn sql >>= \b -> return $ fmap fromSql b

retrieveSingleValue :: IConnection conn => conn -> String -> IO (Maybe SqlValue)
retrieveSingleValue conn sql = fmap firstValue $ quickQuery conn sql []
  where firstValue vs
          | vs == [] = Nothing
          | head vs == [] = Nothing
          | otherwise = Just $ (head . head) vs
