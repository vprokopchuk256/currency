module Str where

import qualified Data.List as List

str :: (Show a) => a -> String
str s = filter ('"' /=) (show s)

join :: (Show a) => [a] -> String
join ss = List.intercalate ", " $ map str ss

(<--) :: (Show a, Show b) => a -> b -> String
a <-- b = List.intercalate " <- " $ filter (not . null) [str a, str b]
