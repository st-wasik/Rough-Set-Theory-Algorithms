module RoughSetTheory where
import Data.List.Split as Split

import Text.ParserCombinators.Parsec
import Data.CSV

import Data.String.Utils

import RoughSetTheory.InfoTable
import RoughSetTheory.InfoObject

import Text.Pretty.Simple

file = "data/girls.csv"
file2 = "data/wd4.csv"

--loadDataFromCsv :: String -> IO [[String]]
loadDataFromCsv path = 
    parseFromFile csvFile path
    >>= return . fmap (fmap (fmap strip))

readDouble :: String -> Double
readDouble = read

main :: IO ()
main = 
    loadDataFromCsv file
    >>= \a -> case a of 
        Left e  -> pPrint e
        Right x -> pPrint $ fromLists x    