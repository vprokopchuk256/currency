import qualified Graph.Graph            as Graph
import qualified Algorithms.BellmanFord as BellmanFord
import qualified Net.FixerIO            as Fixer

main = do
  rates <- Fixer.rates ["USD", "EUR", "GBP"]
  let g = Graph.load rates
  putStrLn $ show g
