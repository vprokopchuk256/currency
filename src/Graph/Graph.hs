module Graph.Graph
(
  Graph
, empty
, size
, vertices
, edges
, addEdge
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Graph.Edge as Edge

type Graph = Map.Map String [Edge.Edge]

empty = Map.empty
size = Map.size
vertices = Map.keys

addEdge :: String -> String -> Float -> Graph -> Graph
addEdge from to rate graph =
  let edge = Edge.new from to rate
  in Map.insertWith (++) from [edge] graph

edges :: Graph -> [Edge.Edge]
edges graph = List.concat $ Map.elems graph
