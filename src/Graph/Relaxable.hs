module Graph.Relaxable ( Relaxable(..) ) where

import qualified Data.HashMap.Strict as Map
import Data.List(intercalate)

data Relaxable a = Tree (Map.HashMap a (a, Float)) | Cycle [a] Float deriving (Eq)

instance (Show a) => Show (Relaxable a) where
    show (Tree mp) = intercalate ", " $ map f $ Map.toList mp
      where f (to, (from, dist)) = intercalate " <- " [(show to), (show dist), (show from)]
    show (Cycle [] p) = ""
    show (Cycle ns p) = (intercalate " <- " $ map show ns) ++ " (" ++ show p ++ ")"
