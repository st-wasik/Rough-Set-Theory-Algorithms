module RoughSetTheory.LEM2 where

import qualified RoughSetTheory.Variant as Variant
import RoughSetTheory.Variant (Variant)

import RoughSetTheory.Rule as Rule

import qualified Data.List as List

import qualified RoughSetTheory.InfoTable as InfoTable
import RoughSetTheory.InfoTable (InfoTable)

import Distinct

-- lem2 :: InfoTable -> [Variant] -> [Rule]
lem2 infoTable approx = rules
    where 
        _X = map (\v -> zipWith (,) attribsNames (Variant.attribs v)) $ approx
        _U = map (\v -> zipWith (,) attribsNames (Variant.attribs v)) $ InfoTable.attribs infoTable
        attribsNames = InfoTable.attribsNames infoTable
        rules = fmap (\t -> Rule.Rule t dec) (lem2_1stLoop _X _X [] _U)
        dec = distinct $ zipWith (,) (repeat $ InfoTable.decisionAttribName infoTable) (fmap Variant.decision approx)

lem2_1stLoop :: [[AttributeValue]] -> [[AttributeValue]] -> [[AttributeValue]] -> [[AttributeValue]] -> [[AttributeValue]]
lem2_1stLoop _G _X totalT _U
    | List.null _G = totalT
    | otherwise = lem2_1stLoop (_G List.\\ newG) _X (totalT ++ [newT]) _U
        where 
            (newT, newG) = lem2_2ndLoop [] _Tg _G _X _U
            _Tg = distinct $ List.concat _G

lem2_2ndLoop :: [AttributeValue] -> [AttributeValue] -> [[AttributeValue]] -> [[AttributeValue]] -> [[AttributeValue]] -> ([AttributeValue], [[AttributeValue]])
lem2_2ndLoop _T _Tg _G _X _U
    | (List.null _T) || _TnotInX = 
        let _Tg_attibs = List.sortBy (\a b -> compare (snd b) (snd a)) $ countOccurences _Tg
            (t, occurences) = head _Tg_attibs
            -- maxOccurencesAttribs = filter ((==) occurences . snd) _Tg_attibs
            newT  = _T ++ [t]
            newG  = (allWith t _G) `List.intersect` _G -- filter (not . allIn newT) _G 
            newTg = (distinct $ List.concat newG) List.\\ newT
        in lem2_2ndLoop newT newTg newG _X _U
    | otherwise = (_T, _G)
    where 
        _TnotInX = not $ allIn (filter (allIn _T) _U) _X
        



countOccurences :: Ord a => [a] -> [(a, Int)]
countOccurences list = fmap (\a -> (head a, length a)) . List.group . List.sort $ list

allIn:: (Functor t1, Foldable t1, Foldable t, Eq a) => t1 a -> t a -> Bool
allIn elems list = List.all (==True) $ fmap (\e -> List.elem e list) elems

allWith :: (Foldable t, Eq a) => a -> [t a] -> [t a]
allWith cond list = filter (List.elem cond) list

