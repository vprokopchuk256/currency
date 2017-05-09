module Graph.Relaxable
    ( Relaxable(..)
    , fromGraph
    ) where

import qualified Data.HashMap.Strict as Map
import Data.List(intercalate)
import Data.Hashable

import Graph.Graph

type Entry a = Maybe (Float, Maybe a)
data Relaxable a =
    Tree (Map.HashMap a (Entry a))
    | Cycle [a] Float
    deriving (Eq)

insert :: (Eq a, Hashable a) => Float -> Maybe a -> a -> Relaxable a -> Relaxable a
insert weight from to (Tree mp) = Tree (Map.insert to (Just (weight, from)) mp)

fromGraph :: (Eq a, Hashable a) => a -> Graph a -> Relaxable a
fromGraph x = init . load
  where
    load g = Tree (Map.fromList $ map (\x -> (x, Nothing)) (vertices g))
    init = insert 0.0 Nothing x

instance (Show a) => Show (Relaxable a) where
    show (Tree mp) = intercalate ", " $ map f $ Map.toList mp
      where
        f (to, Nothing) = show to
        f (to, Just (dist, Nothing)) = intercalate " <- " [(show to), (show dist)]
        f (to, Just (dist, Just from)) = intercalate " <- " [(show to), (show dist), (show from)]
    show (Cycle [] _) = ""
    show (Cycle ns p) = (intercalate " <- " $ map show ns) ++ " (" ++ show p ++ ")"
