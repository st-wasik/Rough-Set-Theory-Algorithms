module RoughSetTheory where
import Data.List.Split as Split

import Text.ParserCombinators.Parsec
import Data.CSV

import Data.String.Utils

import RoughSetTheory.InfoTable as InfoTable
import RoughSetTheory.Variant
import RoughSetTheory.Approximation as Approximation
import RoughSetTheory.LEM2
import RoughSetTheory.Rule
import RoughSetTheory.Reduct

import qualified Data.List as List

import Text.Pretty.Simple

file = "data/girls.csv"
file2 = "data/wd4.csv"
file3 = "data/monk.csv"
file4 = "data/wwd2.csv"
file5 = "data/wwd11.csv"
file6 = "data/test.csv"
file7 = "data/flu.csv"
file8 = "data/wwd11_red.csv"

--loadDataFromCsv :: String -> IO [[String]]
loadDataFromCsv path = 
    parseFromFile csvFile path
    >>= return . fmap (fmap (fmap strip))

readDouble :: String -> Double
readDouble = read

main :: IO ()
main = 
    loadDataFromCsv file8
    >>= exec
    where 
        exec a = case a of
            Left e  -> pPrint e
            Right x -> do 
                let it = fromLists x
                return ()
                print it
                -- print . approximate $ it
                putStrLn " "
                -- print . classificationQuality . approximate $ it
                print $ core $ findReducts it
                -- print $ List.groupBy (\a b -> List.length a == List.length b) . subsets $ InfoTable.attribsNames it
                -- print $ InfoTable.takeAttribs ["bezp."] it
                -- mapM_ print . concatMap (lem2 (InfoTable.attribs it) . Approximation.boundary) . approximate $ it
            --Right x -> let (a,b)=approximate $ fromLists x in pPrint a >> putStrLn "" >> pPrint b
