module Graph.Edge
(
  Edge(from, to, rate, weight)
, new
) where

data Edge = Edge {
  from :: String,
  to :: String,
  rate :: Float,
  weight :: Float
} deriving (Show)

new :: String -> String -> Float -> Edge
new from to rate =
  let weight = -1 * log rate
  in Edge { from = from, to = to, rate = rate, weight = weight }

