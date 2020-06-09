module RoughSetTheory.InfoTable where
import qualified RoughSetTheory.Variant as Variant
import RoughSetTheory.Variant (Variant)

import Data.List

import Control.Applicative

data InfoTable = InfoTable
    { attribsNames :: [String]
    , decisionAttribName :: String
    , attribs      :: [Variant]
    } deriving (Eq, Ord)

instance Show InfoTable where 
    show it = 
        "{" 
        ++ unwords (attribsNames it)
        ++ " => " 
        ++ decisionAttribName it
        ++ "}\n"
        ++ intercalate "\n" (map show $ attribs it)

dropAttribs :: [String] -> InfoTable -> InfoTable
dropAttribs [] it = it 
dropAttribs (a:attrs) it = dropAttribs attrs $ dropAttrib a it

dropAttrib :: String -> InfoTable -> InfoTable
dropAttrib attr it = InfoTable newAttrN (decisionAttribName it) newAttribs
    where 
        newAttrN   = filter (/= attr) $ attribsNames it
        newAttribs = Variant.dropAttrib attr <$> attribs it

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