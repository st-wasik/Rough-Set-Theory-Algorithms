module RoughSetTheory.Reduct
    ( findReducts
    , core
    , showReducts
    , showCore
    ) where 

import qualified Data.Ratio as Ratio
import qualified Data.List as List
import Data.Ratio(Ratio)
import qualified RoughSetTheory.Approximation as Approx 
import RoughSetTheory.Approximation(ClassApproximation)
import qualified RoughSetTheory.InfoTable as InfoTable
import RoughSetTheory.InfoTable(InfoTable)

import Debug.Trace as Debug

showReducts :: [[String]] -> String
showReducts reducts = List.unwords $ f <$> reducts
    where 
        f r = "{" ++ List.unwords r ++ "}"

showCore :: [String] -> String
showCore red = "{" ++ List.unwords red ++ "}"

classificationQuality :: [ClassApproximation] -> Ratio Int
classificationQuality approxs = (all - bound) Ratio.% all
    where 
        all   = sum $ map (length . Approx.upperApprox) approxs
        bound = sum $ map (length . Approx.boundary) approxs

subsets :: [a] -> [[a]]
subsets = List.tail . List.sortBy (\a b -> List.length a `compare` List.length b) . List.subsequences

findReducts :: InfoTable -> [[String]]
findReducts it = checkReducts possibleReducts it baseQuality
    where 
        possibleReducts = List.groupBy (\a b -> List.length a == List.length b) . subsets $ InfoTable.attribsNames it
        baseQuality = classificationQuality . Approx.approximate $ it

checkReducts :: [[[String]]] -> InfoTable -> Ratio Int -> [[String]]
checkReducts [_last] _ _ = _last 
checkReducts (rs:reds) it baseQuality = result
    where 
        qualities = map ((classificationQuality . Approx.approximate) . (`InfoTable.takeAttribs` it)) rs 
        result = let 
            better = map (>= baseQuality) qualities
            in if not $ any (==True) better 
                then checkReducts reds it baseQuality
                else let 
                    indices = List.elemIndices True better
                    in map (rs !!) indices 
            
core :: [[String]] -> [String]
core [] = []
core reds = List.foldl1 List.intersect reds 