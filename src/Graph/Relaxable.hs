module Graph.Relaxable
    ( Relaxable(Tree, Cycle)
    , start
    , relax
    , relaxAll
    , detectCycle
    ) where

import Data.HashMap.Strict(HashMap, insert, fromList, toList, (!))
import Data.Hashable

import Graph.Graph
import Graph.Edge
import Str

type Entry a = Maybe (Float, Maybe a)
type EntryMap a = HashMap a (Entry a)

data Relaxable a =
    Tree (EntryMap a)
    | Cycle [a]
    deriving (Eq)

insert' :: (Eq a, Hashable a) => Float -> Maybe a -> a -> EntryMap a -> EntryMap a
insert' weight from to mp = insert to (Just (weight, from)) mp

start :: (Eq a, Hashable a) => a -> Graph a -> Relaxable a
start x g = Tree ((init . load) g)
  where
    load g = fromList $ map (\x -> (x, Nothing)) (vertices g)
    init = insert' 0.0 Nothing x

relax :: (Eq a, Hashable a) => Edge a -> Relaxable a -> Relaxable a
relax (Edge from to _ weight) t@(Tree mp) = ins (mp ! from) (mp ! to)
  where
    ins Nothing _ = Tree mp
    ins (Just (weightF, _)) Nothing = Tree (insert' (weightF + weight) (Just from) to mp)
    ins (Just (weightF, _)) (Just (weightT, fromT))
        | isRelaxed = Tree (insert' weightN (Just from) to mp)
        | otherwise = Tree mp
      where
        weightN = weightF + weight
        isRelaxed = weightN < weightT

relaxAll :: (Eq a, Hashable a) => [Edge a] -> Relaxable a -> Relaxable a
relaxAll _ c@(Cycle _) = c
relaxAll [] t = t
relaxAll (e:es) t = relaxAll es $ relax e t

tryRelax :: (Eq a, Hashable a) => Edge a -> Relaxable a -> Relaxable a
tryRelax (Edge from to _ weight) (Tree mp) = ins (mp ! from) (mp ! to)
  where
    ins (Just (weightF, _)) (Just (weightT, _))
        | isRelaxed = cycleFrom from (Cycle [])
        | otherwise = Tree mp
      where
        weightN = weightF + weight
        isRelaxed = weightN < weightT
        cycleFrom f (Cycle vs)
            | elem f' vs = cycle
            | otherwise = cycleFrom f' cycle
          where
            Just (_, Just f') = mp ! f
            cycle = Cycle (f':vs)

detectCycle :: (Eq a, Hashable a, Show a) => [Edge a] -> Relaxable a -> Relaxable a
detectCycle _ c@(Cycle _) = c
detectCycle [] t = t
detectCycle (e:es) t = detectCycle es $ tryRelax e t

instance (Show a) => Show (Relaxable a) where
    show (Tree mp) = join $ filter (not . null) $ map f $ toList mp
      where
        f (to, Just (dist, Nothing)) = to <-- dist
        f (to, Just (dist, Just from)) = to <-- dist <-- from
        f (to, m) = to <-- m
    show (Cycle []) = "Empty Cycle"
    show (Cycle ns) = foldl (<--) "" ns
