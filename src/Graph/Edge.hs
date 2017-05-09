module Graph.Edge
    ( Edge(..)
    , new
    ) where

import qualified Data.List as List

data Edge a = Edge
    { from :: a
    , to :: a
    , rate :: Float
    , weight :: Float
    } deriving (Eq)

instance (Show a) => Show (Edge a) where
  show (Edge from to rate _) = List.intercalate " <- " [(show from), (show rate), (show to)]

new :: a -> a -> Float -> (Edge a)
new from to rate = Edge { from = from, to = to, rate = rate, weight = - log rate }

