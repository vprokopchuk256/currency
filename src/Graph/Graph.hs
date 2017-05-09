module Graph.Graph
    ( Graph
    , empty
    , size
    , vertices
    , edges
    , addEdge
    ) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import qualified Graph.Edge as Edge

newtype Graph a = Graph
    { getMap :: Map.HashMap a [Edge.Edge a] }

empty :: Graph a
empty = Graph Map.empty

size :: (Graph a) -> Int
size graph = Map.size $ getMap graph

vertices :: Graph a -> [a]
vertices graph = Map.keys $ getMap graph

edges :: Graph a -> [Edge.Edge a]
edges graph = List.concat $ Map.elems $ getMap graph

addEdge :: (Eq a, Hashable a) => a -> a -> Float -> Graph a -> Graph a
addEdge from to rate (Graph map) =
  let
    edge = Edge.new from to rate
    insert = Map.insertWith (++)
  in
    Graph $ (insert from [edge]) . (insert to []) $ map

instance (Show a) => Show (Graph a) where
  show graph = List.intercalate ", " $ map show $ edges graph
