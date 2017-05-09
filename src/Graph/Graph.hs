module Graph.Graph
    ( Graph
    , empty
    , size
    , vertices
    , edges
    , addEdge
    ) where

import Data.List
import Data.Hashable
import qualified Data.HashMap.Strict as Map

import Graph.Edge

newtype Graph a = Graph
    { getMap :: Map.HashMap a [Edge a] }

empty :: Graph a
empty = Graph Map.empty

size :: (Graph a) -> Int
size = Map.size.getMap

vertices :: Graph a -> [a]
vertices = Map.keys.getMap

edges :: Graph a -> [Edge a]
edges = concat.Map.elems.getMap

addEdge :: (Eq a, Hashable a) => a -> a -> Float -> Graph a -> Graph a
addEdge from to rate g = Graph ((insertFrom.insertTo.getMap) g)
  where
    e = new from to rate
    insert' = Map.insertWith (++)
    insertFrom = insert' from [e]
    insertTo = insert' to []

instance (Show a) => Show (Graph a) where
    show = intercalate'.show'.edges
      where
        intercalate' = intercalate ","
        show' = map show
