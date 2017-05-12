module Algorithms.BellmanFord(execute) where

import Data.Hashable

import Graph.Graph
import Graph.Edge
import Graph.Relaxable

execute :: (Eq a, Hashable a) => a -> Graph a -> Relaxable a
execute v g = relaxAll [e | _ <- vertices g, e <- edges g] (start v g)
