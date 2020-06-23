module RoughSetTheory.Classify
    ( classify
    , classifyVariant
    ) where 

import qualified RoughSetTheory.Variant as Variant
import RoughSetTheory.Variant(Variant, Condition)
import qualified RoughSetTheory.Rule as Rule
import RoughSetTheory.Rule(Rule)
import qualified Data.List as List

classify :: [Rule] -> [Condition] -> [String]
classify rules attribs = map snd $ foldl f [] rules
    where 
        f acc rule = if allIn (Rule.conditions rule) attribs
            then acc ++ Rule.decision rule
            else acc 

classifyVariant :: [Rule] -> Variant -> [Variant]
classifyVariant rules v = result
    where 
        classes = classify rules (Variant.toAttrConditions v)
        dname = if List.null rules then "" else Rule.decisionAttribName $ List.head rules
        result  = fmap (\dec -> Variant.setDecisionAndDName v dec dname) classes

allIn :: Eq a => [a] -> [a] -> Bool
allIn a b = all (==True) $ map (`List.elem` b) a 