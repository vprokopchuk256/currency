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
size g = Map.size $ getMap g

vertices :: Graph a -> [a]
vertices g = Map.keys $ getMap g

edges :: Graph a -> [Edge.Edge a]
edges g = concat $ Map.elems $ getMap g

addEdge :: (Eq a, Hashable a) => a -> a -> Float -> Graph a -> Graph a
addEdge from to rate (Graph g) =
    Graph $ (f from [e]) . (f to []) $ g
  where
    e = Edge.new from to rate
    f = Map.insertWith (++)

instance (Show a) => Show (Graph a) where
  show g = List.intercalate ", " $ map show $ edges g
