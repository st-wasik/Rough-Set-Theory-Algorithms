module RoughSetTheory.Approximation where 
import qualified Data.Map as Map
import Data.Map(Map)

import qualified RoughSetTheory.InfoTable as InfoTable
import RoughSetTheory.InfoTable(InfoTable)
import qualified RoughSetTheory.InfoObject as InfoObject
import RoughSetTheory.InfoObject(InfoObject)

import Data.Maybe
import Data.List

data ClassApproximation = ClassApproximation 
    { attribsNames :: [String]
    , decisionAttribName :: String
    , className :: String
    , lowerApprox :: [InfoObject]
    , upperApprox :: [InfoObject]
    , boundaryRegion :: [InfoObject]
    } 

instance Show ClassApproximation where 
    show a = 
        "Approximations for class {"
        ++ className a
        ++ "}:\n"
        ++ ""
        ++ show (InfoObject.InfoObject (attribsNames a) (decisionAttribName a))
        ++ ""
        ++ "\n<Upper approx>:\n"
        ++ intercalate "\n" (map (("-> "++) . show) $ upperApprox a)
        ++ "\n\n<Boundary region>:\n"
        ++ intercalate "\n" (map (("-> "++) . show) $ boundaryRegion a)
        ++ "\n\n<Lower approx>:\n"
        ++ intercalate "\n" (map (("-> "++) . show) $ lowerApprox a)

approximateClass it className classToAttrs attrsToClass = 
    ClassApproximation (InfoTable.attribsNames it) (InfoTable.decisionAttribName it) className (nub $ lower) (nub $ lower ++ upper) (upper \\ lower)
        where 
            Just attrs = Map.lookup className classToAttrs  
            (lower, upper) = approximateClass' className attrs attrsToClass [] []

approximateClass' _ [] _ lower upper = (lower, upper)
approximateClass' className (a:attrs) attrsToClass lApx uApx =
    approximateClass' className attrs attrsToClass newLAppx newUAppx
    where 
        Just classesForAttrs = Map.lookup a attrsToClass  
        (newLAppx, newUAppx) = if all (==className) classesForAttrs
            then (lApx ++ [InfoObject.InfoObject a className], uApx)
            else (lApx, uApx ++ [InfoObject.InfoObject a className])



approximate it = result
    where 
        (a, b) = buildApproxMaps it
        classes = nub $ Map.keys a
        result = map (\cls -> approximateClass it cls a b) classes


buildApproxMaps :: InfoTable -> (Map String [[String]], Map [String] [String]) 
buildApproxMaps it = buildApproxMaps' (InfoTable.attribs it) Map.empty Map.empty

buildApproxMaps' [] classToAttrs attrsToClass = (classToAttrs, attrsToClass)
buildApproxMaps' (x:xs) classToAttrs attrsToClass = 
    buildApproxMaps' xs newClassToAttrs newAttrsToClass
    where 
        dec   = InfoObject.decision x
        attrs = InfoObject.attribs  x 
        newClassToAttrs = Map.insertWith' (++) dec [attrs] classToAttrs 
        newAttrsToClass = Map.insertWith' (++) attrs [dec] attrsToClass

same [] = True
same [_] = True
same (x:xs) = all (==x) xs