module Algorithms.BellmanFord(execute) where

import qualified Graph.Graph as Graph
import qualified Algorithms.Results as Results

execute :: Graph.Graph -> String -> Results.Set
execute graph i =
  let initials = Results.initial graph i
      vertices = Graph.vertices graph
      edges = Graph.edges graph
  in foldl Results.relax initials [e | _ <- vertices, e <- edges]
