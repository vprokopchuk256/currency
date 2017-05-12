module Graph.Graph
    ( Graph
    , empty
    , size
    , vertices
    , edges
    , (<<<)
    , addEdge
    ) where

import Data.List
import Data.Hashable
import qualified Data.HashMap.Strict as Map

import Graph.Edge
import Str

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

(<<<) :: (Eq a, Hashable a) => Graph a -> Edge a -> Graph a
g <<< e = Graph ((insertFrom.insertTo.getMap) g)
  where
    insert' = Map.insertWith (++)
    insertFrom = insert' (from e) [e]
    insertTo = insert' (to e) []

addEdge :: (Eq a, Hashable a) => a -> a -> Float -> Graph a -> Graph a
addEdge from to rate g = g <<< edge from to rate

instance (Show a) => Show (Graph a) where
    show = join . edges
