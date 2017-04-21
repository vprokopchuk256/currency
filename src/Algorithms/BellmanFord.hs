module Algorithms.BellmanFord(
  execute
) where

import qualified Data.Map as Map
import qualified Graph.Graph as Graph

data Result = Result { dist :: Float, prev :: String } deriving (Show)
type Results = Map.Map String Result

emptyResult = Result { dist = 10000.0, prev = "" }
initialResult = Result { dist = 1.0, prev = "" }

initialResults :: Graph.Graph -> String -> Results
initialResults graph initial =
  let v = filter (initial /=) $ Graph.vertices graph
      l = zip v [emptyResult]
  in Map.insert initial initialResult $ Map.fromList l

execute :: Graph.Graph -> String -> Results
execute = initialResults


