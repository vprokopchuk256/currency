module Graph.Relaxable ( Relaxable(..) ) where

import qualified Data.HashMap.Strict as Map
import Data.List(intercalate)

type Entry a = Maybe (Maybe a, Float)
data Relaxable a = Tree (Map.HashMap a (Entry a)) | Cycle [a] Float deriving (Eq)

instance (Show a) => Show (Relaxable a) where
    show (Tree mp) = intercalate ", " $ map f $ Map.toList mp
      where
        f (to, Nothing) = show to
        f (to, Just (Nothing, dist)) = intercalate " <- " [(show to), (show dist)]
        f (to, Just (Just from, dist)) = intercalate " <- " [(show to), (show dist), (show from)]
    show (Cycle [] _) = ""
    show (Cycle ns p) = (intercalate " <- " $ map show ns) ++ " (" ++ show p ++ ")"
