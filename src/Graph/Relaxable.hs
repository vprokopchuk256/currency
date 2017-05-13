module Graph.Relaxable
    ( Relaxable(Tree, Cycle)
    , tree
    , relax
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

tree :: (Eq a, Hashable a) => a -> Graph a -> Relaxable a
tree x g = Tree ((init . load) g)
  where
    load g = fromList $ map (\x -> (x, Nothing)) (vertices g)
    init = insert' 0.0 Nothing x

cycle' :: (Eq a, Hashable a) => a -> Relaxable a -> Relaxable a
cycle' x (Tree mp) = cycleFrom x (Cycle [])
  where
    cycleFrom f (Cycle vs)
        | elem f' vs = c
        | otherwise = cycleFrom f' c
      where
        Just (_, Just f') = mp ! f
        c = Cycle (f':vs)

relaxEdge :: (Eq a, Hashable a) => Relaxable a -> Edge a -> Maybe (Relaxable a)
relaxEdge (Tree mp) (Edge from to _ weight) = ins (mp ! from) (mp ! to)
  where
    ins Nothing _ = Nothing
    ins (Just (weightF, _)) Nothing = Just (Tree (insert' (weightF + weight) (Just from) to mp))
    ins (Just (weightF, _)) (Just (weightT, _))
        | weightN < weightT = Just (Tree (insert' weightN (Just from) to mp))
        | otherwise = Nothing
      where
        weightN = weightF + weight

relax :: (Eq a, Hashable a) => Relaxable a -> [Edge a] -> Relaxable a
relax = foldl relaxEdge'
  where
    relaxEdge' t e = maybe t id $ relaxEdge t e

detectCycle :: (Eq a, Hashable a) => [Edge a] -> Relaxable a -> Relaxable a
detectCycle _ c@(Cycle _) = c
detectCycle [] t = t
detectCycle (e:es) t = detectCycle es $ relaxEdge' t e
  where
    relaxEdge' t e@(Edge from _ _ _) = maybe t (\t' -> cycle' from t) $ relaxEdge t e

instance (Show a) => Show (Relaxable a) where
    show (Tree mp) = join $ filter (not . null) $ map f $ toList mp
      where
        f (to, Just (dist, Nothing)) = to <-- dist
        f (to, Just (dist, Just from)) = to <-- dist <-- from
        f (to, m) = to <-- m
    show (Cycle []) = "Empty Cycle"
    show (Cycle ns) = foldl (<--) "" ns
