module Distinct
    ( distinct
    ) where

import qualified Data.List as List
import qualified Data.Set  as Set

distinct :: Ord a => [a] -> [a]
distinct list = snd $ foldl f (Set.empty, []) list
    where 
        f (set, acc) e
            | e `Set.member` set = (set, acc)
            | otherwise = (e `Set.insert` set, acc ++ [e])