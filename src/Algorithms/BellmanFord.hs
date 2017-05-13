module Algorithms.BellmanFord(execute) where

import Data.Hashable

import Graph.Graph
import Graph.Edge
import Graph.Relaxable

execute' :: (Eq a, Hashable a, Show a) => Relaxable a -> [Edge a] -> [a] -> Relaxable a
execute' r es [v] = detectCycle es r
execute' r es (_:vs) = execute' (relax r es) es vs

execute :: (Eq a, Hashable a, Show a) => a -> Graph a -> Relaxable a
execute v g = execute' (start v g) (edges g) (vertices g)
