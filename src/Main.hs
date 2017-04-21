import qualified Graph.Graph as Graph
import qualified Algorithms.BellmanFord as BellmanFord

main = do
  let graph= Graph.addEdge "USD" "EUR" 0.5 .
             Graph.addEdge "USD" "USD" 1.0 .
             Graph.addEdge "EUR" "USD" 2.0 .
             Graph.addEdge "EUR" "EUR" 1.0 $ Graph.empty
  putStrLn (show $ BellmanFord.execute graph "USD")
