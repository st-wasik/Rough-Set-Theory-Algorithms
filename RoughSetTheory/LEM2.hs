module RoughSetTheory.LEM2 where

import qualified RoughSetTheory.Variant as Variant
import RoughSetTheory.Variant (Variant, Condition)

import RoughSetTheory.Rule as Rule

import qualified Data.List as List

import qualified RoughSetTheory.InfoTable as InfoTable
import RoughSetTheory.InfoTable (InfoTable)

import Distinct

conditionsOf :: [Variant] -> [Variant.Condition]
conditionsOf x = List.nub $ concatMap Variant.toAttrConditions x

lem2 :: [Variant] -> [Variant] -> [Rule]
lem2 _U _X = let 
    rules = lem2_1st_loop _X _X _U []
    newRules = dropUnnecessaryRules rules _X _U
    in makeRules newRules _U

makeRules :: [[Condition]] -> [Variant] -> [Rule]
makeRules conds _U = List.foldl' f [] conds
    where 
        f acc c = let 
            variants = variantsForConditions c _U
            d = List.nub . map Variant.toDecCondition $ variants
            in acc ++ [Rule c d]

lem2_1st_loop :: [Variant] -> [Variant] -> [Variant] -> [[Condition]] -> [[Condition]]
lem2_1st_loop _G _X _U rules
    | null _G = rules 
    | otherwise = let 
        _T  = []  
        _Tg = conditionsOf _G
        conds = lem2_2nd_loop _T _Tg _G _X _U
        newRules = rules ++ [dropUnnecessaryConds conds _X _U]
        newG = _X List.\\ concatMap (`variantsForConditions` _U) newRules
        in lem2_1st_loop newG _X _U newRules

lem2_2nd_loop :: [Condition] -> [Condition] -> [Variant] -> [Variant] -> [Variant] -> [Condition]
lem2_2nd_loop _T _Tg _G _X _U 
    | null _T || not (variantsForConditions _T _U `allIn` _X) = let 
        condition = findBestCondition _Tg _G _U
        newT  = List.union _T [condition]
        newG  = variantsForConditions [condition] _G `List.intersect` _G
        newTg = conditionsOf newG List.\\ newT
        in lem2_2nd_loop newT newTg newG _X _U
    | otherwise = _T

findBestCondition :: [Condition] -> [Variant] -> [Variant] -> Condition
findBestCondition _Tg _G _U = result
    where 
        allConds = concatMap Variant.toAttrConditions _G
        correctConds = filter (`List.elem` _Tg) allConds
        freqOfConds = countOccurences correctConds
        sortedConds = List.sortBy (\(_,a) (_,b) -> compare b a) freqOfConds
        maxOccurences = snd . List.head $ sortedConds
        bestConds = fmap fst . filter (\a -> snd a == maxOccurences) $ sortedConds
        result = case bestConds of 
            [a]  -> a 
            list -> findLeastCoveringCondition list _U

findLeastCoveringCondition :: [Condition] -> [Variant] -> Condition
findLeastCoveringCondition conds _U = result
    where 
        variantsFreq = let 
            count a = length $ [a] `variantsForConditions` _U 
            in fmap (\ a -> (a, count a)) conds
        minimum = snd $ List.minimumBy (\a b -> snd a `compare` snd b) variantsFreq
        result = fst . head . filter (\a -> snd a == minimum) $ variantsFreq

dropUnnecessaryConds :: [Condition] -> [Variant] -> [Variant] -> [Condition]
dropUnnecessaryConds _T _X _U = dropUnnecessaryConds' _T _X _U []

dropUnnecessaryConds' :: [Condition] -> [Variant] -> [Variant] -> [Condition] -> [Condition]
dropUnnecessaryConds' [] _ _ acc = acc
dropUnnecessaryConds' (t:_T) _X _U acc
    | variantsForConditions (acc ++ _T) _U `allIn` _X = dropUnnecessaryConds' _T _X _U acc
    | otherwise = dropUnnecessaryConds' _T _X _U (acc ++ [t])

dropUnnecessaryRules :: [[Condition]] -> [Variant] -> [Variant] -> [[Condition]]
dropUnnecessaryRules rules _X _U = dropUnnecessaryRules' rules (List.sort $ List.nub _X) _U []

dropUnnecessaryRules' :: [[Condition]] -> [Variant] -> [Variant] -> [[Condition]] -> [[Condition]]
dropUnnecessaryRules' [] _ _ acc = acc
dropUnnecessaryRules' (r:rules) _X _U acc
    | coveredVariants == _X = dropUnnecessaryRules' rules _X _U acc
    | otherwise = dropUnnecessaryRules' rules _X _U (acc ++ [r])
    where coveredVariants = List.sort . List.nub $ concatMap (`variantsForConditions` _U) (acc ++ rules)

variantsForConditions :: [Condition] -> [Variant] -> [Variant]
variantsForConditions cond = filter f
    where f = allIn cond . Variant.toAttrConditions  

countOccurences :: Ord a => [a] -> [(a, Int)]
countOccurences = fmap (\a -> (head a, length a)) . List.group . List.sort
    
allIn :: Eq a => [a] -> [a] -> Bool
allIn elems list = List.intersect elems list == elems
