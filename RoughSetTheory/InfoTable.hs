module RoughSetTheory.InfoTable where
import qualified RoughSetTheory.Variant as Variant
import RoughSetTheory.Variant (Variant)

import Data.List as List

import Control.Applicative

data InfoTable = InfoTable
    { attribsNames :: [String]
    , decisionAttribName :: String
    , variants      :: [Variant]
    } deriving (Eq, Ord)

instance Show InfoTable where 
    show it = 
        intercalate "\n" (map show $ variants it)

variantsForClasses :: InfoTable -> [String] -> [Variant]
variantsForClasses it classes = filter (\v -> Variant.decision v `List.elem` classes) $ variants it

dropAttribs :: [String] -> InfoTable -> InfoTable
dropAttribs [] it = it 
dropAttribs (a:attrs) it = dropAttribs attrs $ dropAttrib a it

dropAttrib :: String -> InfoTable -> InfoTable
dropAttrib attr it = InfoTable newAttrN (decisionAttribName it) newVariants
    where 
        newAttrN   = filter (/= attr) $ attribsNames it
        newVariants = Variant.dropAttrib attr <$> variants it

takeAttribs :: [String] -> InfoTable -> InfoTable
takeAttribs attrs it = dropAttribs newAttrs it
    where
        newAttrs = attribsNames it \\ attrs

fromLists :: [[String]] -> InfoTable
fromLists input = InfoTable attrNames decisionAttrName attrsValues
    where 
        allAttrNames = head input
        attrNames = let n = init allAttrNames in if head n == "name" then tail n else n
        decisionAttrName = last allAttrNames 
        attrsValues = map (let n = init allAttrNames in if head n == "name" then Variant.fromListWithName allAttrNames else Variant.fromList allAttrNames) $ drop 1 input

unclassifiedFromLists :: [[String]] -> InfoTable
unclassifiedFromLists input = InfoTable attrNames "" attrsValues
    where 
        allAttrNames = head input
        attrNames = let n = allAttrNames in if head n == "name" then tail n else n
        attrsValues = map (let n = allAttrNames in if head n == "name" then Variant.unclassifiedFromListWithName allAttrNames else Variant.unclassifiedFromList allAttrNames) $ drop 1 input