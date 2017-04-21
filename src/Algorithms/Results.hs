module Algorithms.Results(Set, initial) where

import qualified Graph.Graph as Graph
import qualified Graph.Edge as Edge
import qualified Data.Map as Map

data Entry = Entry { dist :: Float, prev :: String } deriving (Show)
type Set = Map.Map String Entry

emptyResult = Entry { dist = 10000.0, prev = "" }
initialResult = Entry { dist = 1.0, prev = "" }

initial :: Graph.Graph -> String -> Set
initial graph i =
  let v = filter (i /=) $ Graph.vertices graph
      l = zip v [emptyResult]
  in Map.insert i initialResult $ Map.fromList l

relax :: Set -> Edge.Edge -> Set
relax set edge
  | newDist < oldDist = Map.insert to Entry { dist = newDist, prev = from } set
  | otherwise = set
  where from = Edge.from edge
        to = Edge.to edge
        weight = Edge.weight edge
        newDist = dist ((Map.!) set from) + weight
        oldDist = dist ((Map.!) set to)
