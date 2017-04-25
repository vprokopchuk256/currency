module Graph.Edge
    ( Edge(..)
    , new
    ) where

import qualified Data.List as List

data Edge = Edge
    { from :: String
    , to :: String
    , rate :: Float
    , weight :: Float
    } deriving (Eq)

instance Show Edge where
  show (Edge from to rate _) = List.intercalate " <- " [from, show rate, to]

new :: String -> String -> Float -> Edge
new from to rate = Edge { from = from, to = to, rate = rate, weight = - log rate }

