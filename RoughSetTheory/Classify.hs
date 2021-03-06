module RoughSetTheory.Classify
    ( classify
    , classifyVariant
    ) where 

import qualified RoughSetTheory.Variant as Variant
import RoughSetTheory.Variant(Variant, Condition)
import qualified RoughSetTheory.Rule as Rule
import RoughSetTheory.Rule(Rule)

import qualified Data.Ratio as Ratio
import Data.Ratio(Ratio)
import qualified Data.List as List

classify :: [Rule] -> [Condition] -> [String]
classify rules attribs = if List.null bestResult 
    then classifyNotFullMatch rules attribs
    else bestResult 
    where 
        bestResult = if List.null sortedClassified then [] else [(snd . snd) (head sortedClassified)]
        sortedClassified = List.sortBy (\a b -> compare (fst b) (fst a)) fullyClassified
        fullyClassified = foldl f [] rules
        f acc rule = if allIn (Rule.conditions rule) attribs
            then acc ++ fmap ((,) (Rule.support rule)) (Rule.decision rule)
            else acc 
        

classifyVariant :: [Rule] -> Variant -> [Variant]
classifyVariant rules v = result
    where 
        classes = classify rules (Variant.toAttrConditions v)
        dname = if List.null rules then "" else Rule.decisionAttribName $ List.head rules
        result  = fmap (\dec -> Variant.setDecisionAndDName v dec dname) classes


allIn :: Eq a => [a] -> [a] -> Bool
allIn a b = all (==True) $ map (`List.elem` b) a 

classifyNotFullMatch :: [Rule] -> [Condition] -> [String]
classifyNotFullMatch rules attribs = result
    where 
        covered = List.sortBy (\a b -> compare (fst b) (fst a)) . filter (\a -> 0 < fst a) . map (`conditionCoverage` attribs) $ rules
        result = if List.null covered 
            then [] 
            else 
                let maxVal = fst (head covered) 
                in List.nub . concatMap snd . filter ((==) maxVal . fst) $ covered

conditionCoverage :: Rule -> [Condition] -> (Ratio Int, [String]) 
conditionCoverage r attribs = (List.length covered Ratio.% List.length ruleConds, decision) 
    where 
        decision = snd <$> Rule.decision r
        ruleConds = Rule.conditions r
        covered = ruleConds `List.intersect` attribs
