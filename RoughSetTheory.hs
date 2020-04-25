module RoughSetTheory where
import Data.List.Split as Split

import Text.ParserCombinators.Parsec
import Data.CSV

import Data.String.Utils

import RoughSetTheory.InfoTable
import RoughSetTheory.InfoObject
import RoughSetTheory.Approximation

import Text.Pretty.Simple

file = "data/girls.csv"
file2 = "data/wd4.csv"
file3 = "data/monk.csv"

--loadDataFromCsv :: String -> IO [[String]]
loadDataFromCsv path = 
    parseFromFile csvFile path
    >>= return . fmap (fmap (fmap strip))

readDouble :: String -> Double
readDouble = read

main :: IO ()
main = 
    loadDataFromCsv file2
    >>= \a -> case a of 
        Left e  -> pPrint e
        Right x -> do 
            let it = fromLists x
            pPrint it
            pPrint . approximate $ it
        --Right x -> let (a,b)=approximate $ fromLists x in pPrint a >> putStrLn "" >> pPrint b   