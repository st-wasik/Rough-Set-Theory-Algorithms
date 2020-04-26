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
        ++ " => " 
        ++ (decisionAttribName it) 
        ++ "}\n"
        ++ intercalate "\n" (map show $ attribs it)

fromLists :: [[String]] -> InfoTable
fromLists input = InfoTable attrNames decisionAttrName attrsValues
    where 
        allAttrNames = head input
        attrNames = let n = init allAttrNames in if head n == "name" then tail n else n
        decisionAttrName = last allAttrNames 
        attrsValues = map (let n = init allAttrNames in if head n == "name" then InfoObject.fromListWithName else InfoObject.fromList) $ drop 1 input
