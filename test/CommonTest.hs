module CommonTest
    ( putStrLnSuccess,
      putStrLnFailure
    ) where

putStrLnSuccess :: String -> IO ()
putStrLnSuccess s = putStrLn $ "\x1b[32m" ++ s ++ "\x1b[0m"

putStrLnFailure :: String -> IO ()
putStrLnFailure s = putStrLn $ "\x1b[31m" ++ s ++ "\x1b[0m"
