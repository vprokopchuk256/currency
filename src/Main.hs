import qualified Graph.Graph            as Graph
import qualified Algorithms.BellmanFord as BellmanFord
import qualified Net.FixerIO            as Fixer

main = do
  rates <- Fixer.rates ["USD", "EUR", "GBP"]
  putStrLn $ show rates
