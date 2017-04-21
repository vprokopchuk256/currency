import qualified Graph.Graph as Graph
import qualified Algorithms.BellmanFord as BellmanFord

main = do
  let graph= Graph.addEdge "USD" "USD" 1.000 .
             Graph.addEdge "USD" "EUR" 0.741 .
             Graph.addEdge "USD" "GBP" 0.657 .
             Graph.addEdge "USD" "CHF" 1.061 .
             Graph.addEdge "USD" "CAD" 1.011 .
             Graph.addEdge "EUR" "USD" 1.350 .
             Graph.addEdge "EUR" "EUR" 1.000 .
             Graph.addEdge "EUR" "GBP" 0.888 .
             Graph.addEdge "EUR" "CHF" 1.433 .
             Graph.addEdge "EUR" "CAD" 1.366 .
             Graph.addEdge "GBP" "USD" 1.521 .
             Graph.addEdge "GBP" "EUR" 1.126 .
             Graph.addEdge "GBP" "GBP" 1.000 .
             Graph.addEdge "GBP" "CHF" 1.614 .
             Graph.addEdge "GBP" "CAD" 1.538 .
             Graph.addEdge "CHF" "USD" 1.943 .
             Graph.addEdge "CHF" "EUR" 0.698 .
             Graph.addEdge "CHF" "GBP" 0.620 .
             Graph.addEdge "CHF" "CHF" 1.000 .
             Graph.addEdge "CHF" "CAD" 0.953 .
             Graph.addEdge "CAD" "USD" 0.995 .
             Graph.addEdge "CAD" "EUR" 0.732 .
             Graph.addEdge "CAD" "GBP" 0.650 .
             Graph.addEdge "CAD" "CHF" 1.049 .
             Graph.addEdge "CAD" "CAD" 1.000 $ Graph.empty
  putStrLn (show $ BellmanFord.execute graph "USD")
