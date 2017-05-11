module Graph.Relaxable
    ( Relaxable(Tree, Cycle)
    , start
    , relax
    ) where

import qualified Data.HashMap.Strict as Map
import Data.Hashable
import qualified Debug.Trace as Trace

import Graph.Graph
import Graph.Edge
import Str

type Entry a = Maybe (Float, Maybe a)
type EntryMap a = Map.HashMap a (Entry a)

data Relaxable a =
    Tree (EntryMap a)
    | Cycle [a] Float
    deriving (Eq)

insert' :: (Eq a, Hashable a) => Float -> Maybe a -> a -> EntryMap a -> EntryMap a
insert' weight from to mp = Map.insert to (Just (weight, from)) mp

start :: (Eq a, Hashable a) => a -> Graph a -> Relaxable a
start x g = Tree ((init . load) g)
  where
    load g = Map.fromList $ map (\x -> (x, Nothing)) (vertices g)
    init = insert' 0.0 Nothing x

relax :: (Eq a, Hashable a, Show a) => Edge a -> Relaxable a -> Relaxable a
relax (Edge from to _ weight) t@(Tree mp) = ins ((Map.!) mp from) ((Map.!) mp to)
  where
    ins Nothing _ = Tree mp
    ins (Just (weightF, _)) Nothing = Tree (insert' (weightF + weight) (Just from) to mp)
    ins (Just (weightF, _)) (Just (weightT, fromT))
        | isRelaxed && isCycle = Cycle [] 0.0
        | isRelaxed = Tree (insert' weightN (Just from) to mp)
        | otherwise = Tree mp
      where
        weightN = weightF + weight
        isRelaxed = weightN < weightT
        isCycle = (Just from) == fromT

instance (Show a) => Show (Relaxable a) where
    show (Tree mp) = join $ filter (not . null) $ map f $ Map.toList mp
      where
        f (to, Just (dist, Nothing)) = to <-- dist
        f (to, Just (dist, Just from)) = to <-- dist <-- from
        f (to, m) = to <-- m
    show (Cycle [] _) = "Empty Cycle"
    show (Cycle ns p) = (foldl (<--) "" ns) ++ " (" ++ str p ++ ")"
