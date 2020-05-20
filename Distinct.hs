module Distinct
    ( distinct
    ) where

import qualified Data.List as List
import qualified Data.Set  as Set

distinct :: Ord a => [a] -> [a]
distinct list 
    | (length list) < 100 = List.nub list
    | otherwise = distinct' list [] Set.empty

distinct' :: Ord a => [a] -> [a] -> Set.Set a -> [a]
distinct' [] out set = List.reverse out
distinct' (x:xs) out set
    | x `Set.member` set = distinct' xs out set
    | otherwise = distinct' xs (x:out) (x `Set.insert` set) 