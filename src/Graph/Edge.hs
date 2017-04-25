module Graph.Edge
(
  Edge(..)
, new
) where

data Edge = Edge {
  from :: String,
  to :: String,
  rate :: Float,
  weight :: Float
} deriving (Show)

new :: String -> String -> Float -> Edge
new from to rate = Edge { from = from, to = to, rate = rate, weight = - log rate }

