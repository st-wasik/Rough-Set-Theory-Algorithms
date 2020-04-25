module RoughSetTheory.InfoTable where
import qualified RoughSetTheory.InfoObject as InfoObject
import RoughSetTheory.InfoObject (InfoObject)

import Data.List

data InfoTable = InfoTable
    { attribsNames :: [String]
    , decisionAttribName :: String
    , attribs      :: [InfoObject]
    } deriving (Eq, Ord)

instance Show InfoTable where 
    show it = 
        "{" 
        ++ (intercalate " " $ attribsNames it) 
        ++ " : " 
        ++ (decisionAttribName it) 
        ++ "}\n"
        ++ intercalate "\n" (map show $ attribs it)

fromLists :: [[String]] -> InfoTable
fromLists input = InfoTable attrNames decisionAttrName attrsValues
    where 
        allAttrNames = head input
        attrNames = init allAttrNames
        decisionAttrName = last allAttrNames 
        attrsValues = map InfoObject.fromList $ drop 1 input
