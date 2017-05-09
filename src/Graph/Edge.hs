module Graph.Edge
    ( Edge(..)
    , new
    ) where

import qualified Data.List as List

import Graph.VertexName

data Edge = Edge
    { from :: VertexName
    , to :: VertexName
    , rate :: Float
    , weight :: Float
    } deriving (Eq)

instance Show Edge where
  show (Edge from to rate _) = List.intercalate " <- " [from, show rate, to]

new :: VertexName -> VertexName -> Float -> Edge
new from to rate = Edge { from = from, to = to, rate = rate, weight = - log rate }

