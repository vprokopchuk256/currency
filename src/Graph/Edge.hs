module Graph.Edge
    ( Edge(..)
    , edge
    ) where

import Str

data Edge a = Edge
    { from :: a
    , to :: a
    , rate :: Float
    , weight :: Float
    } deriving (Eq)

instance (Show a) => Show (Edge a) where
  show (Edge from to rate _) = from <-- rate <-- to

edge :: a -> a -> Float -> (Edge a)
edge from to rate = Edge from to rate (- log rate)

