import UnitTests (runUnitTests)
import IntegrationTests (runIntegrationTests)

main :: IO ()
main = do
  runUnitTests
  putStrLn ""
  runIntegrationTests
