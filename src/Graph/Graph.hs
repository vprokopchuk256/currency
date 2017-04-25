module Graph.Graph
    ( empty
    , size
    , vertices
    , edges
    , addEdge
    ) where

import qualified Data.Map as Map
import qualified Data.List as List

import qualified Graph.Edge as Edge

type VertexName = String
newtype Graph = Graph
    { getMap :: Map.Map VertexName [Edge.Edge] }

empty :: Graph
empty = Graph Map.empty

size :: Graph -> Int
size graph = Map.size $ getMap graph

vertices :: Graph -> [VertexName]
vertices graph = Map.keys $ getMap graph

edges :: Graph -> [Edge.Edge]
edges graph = List.concat $ Map.elems $ getMap graph

addEdge :: VertexName -> VertexName -> Float -> Graph -> Graph
addEdge from to rate (Graph map) =
  let
    edge = Edge.new from to rate
    insert = Map.insertWith (++)
  in
    Graph $ (insert from [edge]) . (insert to []) $ map

instance Show Graph where
  show graph = List.intercalate ", " $ map show $ edges graph
