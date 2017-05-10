module Graph.Relaxable
    ( Relaxable(..)
    , fromGraph
    , relax
    ) where

import qualified Data.HashMap.Strict as Map
import Data.List(intercalate)
import Data.Hashable

import Graph.Graph
import Graph.Edge

type Entry a = Maybe (Float, Maybe a)
type EntryMap a = Map.HashMap a (Entry a)

data Relaxable a =
    Tree (EntryMap a)
    | Cycle [a] Float
    deriving (Eq)

insert' :: (Eq a, Hashable a) => Float -> Maybe a -> a -> EntryMap a -> EntryMap a
insert' weight from to mp = Map.insert to (Just (weight, from)) mp

fromGraph :: (Eq a, Hashable a) => a -> Graph a -> Relaxable a
fromGraph x g = Tree ((init . load) g)
  where
    load g = Map.fromList $ map (\x -> (x, Nothing)) (vertices g)
    init = insert' 0.0 Nothing x

relax :: (Eq a, Hashable a) => Edge a -> Relaxable a -> Relaxable a
relax (Edge f t _ w) (Tree mp) = Tree (insert' w (Just f) t mp)

instance (Show a) => Show (Relaxable a) where
    show (Tree mp) = intercalate ", " $ map f $ Map.toList mp
      where
        f (to, Nothing) = show to
        f (to, Just (dist, Nothing)) = intercalate " <- " [(show to), (show dist)]
        f (to, Just (dist, Just from)) = intercalate " <- " [(show to), (show dist), (show from)]
    show (Cycle [] _) = ""
    show (Cycle ns p) = (intercalate " <- " $ map show ns) ++ " (" ++ show p ++ ")"
