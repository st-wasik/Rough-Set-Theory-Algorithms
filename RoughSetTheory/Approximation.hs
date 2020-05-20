module RoughSetTheory.Approximation where 
import qualified Data.Map as Map
import Data.Map(Map)

import qualified RoughSetTheory.InfoTable as InfoTable
import RoughSetTheory.InfoTable(InfoTable)
import qualified RoughSetTheory.Variant as Variant
import RoughSetTheory.Variant(Variant)

import Data.Maybe
import Data.List

data ClassApproximation = ClassApproximation 
    { attribsNames :: [String]
    , decisionAttribName :: String
    , className :: String
    , lowerApprox :: [Variant]
    , upperApprox :: [Variant]
    -- , boundaryRegion :: [Variant]
    } 

instance Show ClassApproximation where 
    show a = 
        "\n\nApproximations for class {"
        ++ className a
        ++ "} with attributes {"
        ++ ""
        ++ intercalate " " (attribsNames a)
        ++ " => "
        ++ decisionAttribName a
        ++ "}"
        ++ "\n<Upper approx.>:\n"
        ++ intercalate "\n" (map (("  -> "++) . show) $ upperApprox a)
        -- ++ "\n\n<Boundary region>:\n"
        -- ++ intercalate "\n" (map (("-> "++) . show) $ boundaryRegion a)
        ++ "\n\n<Lower approx.>:\n"
        ++ intercalate "\n" (map (("  -> "++) . show) $ lowerApprox a)
        ++ ""

approximateClass it className classToAttrs attrsToClass = 
    ClassApproximation (InfoTable.attribsNames it) (InfoTable.decisionAttribName it) className (nub $ lower) (nub $ lower ++ upper) --(nub $ upper \\ lower)
        where 
            Just attrs = Map.lookup className classToAttrs  
            (lower, upper) = approximateClass' className attrs attrsToClass [] []

approximateClass' _ [] _ lower upper = (lower, upper)
approximateClass' className (a:attrs) attrsToClass lApx uApx =
    approximateClass' className attrs attrsToClass newLAppx newUAppx
    where 
        Just classesForAttrs = Map.lookup (Variant.attribs a) attrsToClass  
        (newLAppx, newUAppx) = if all (\d -> (Variant.decision d) == className) classesForAttrs
            then (a:lApx, uApx)
            else (lApx, (reverse classesForAttrs) ++ uApx)



approximate it = result
    where 
        (a, b) = buildApproxMaps it
        classes = nub $ Map.keys a
        result = map (\cls -> approximateClass it cls a b) classes


buildApproxMaps :: InfoTable -> (Map String [Variant], Map [String] [Variant]) 
buildApproxMaps it = buildApproxMaps' (InfoTable.attribs it) Map.empty Map.empty

buildApproxMaps' [] classToAttrs attrsToClass = (classToAttrs, attrsToClass)
buildApproxMaps' (x:xs) classToAttrs attrsToClass = 
    buildApproxMaps' xs newClassToAttrs newAttrsToClass
    where 
        dec   = Variant.decision x
        attrs = Variant.attribs  x 
        newClassToAttrs = Map.insertWith' (++) dec [x] classToAttrs 
        newAttrsToClass = Map.insertWith' (++) attrs [x] attrsToClass

same [] = True
same [_] = True
same (x:xs) = all (==x) xs