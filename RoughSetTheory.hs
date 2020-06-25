module RoughSetTheory
    ( dataExample
    , approximationExample
    , lem2Example
    , reductExample
    , classifyExample   
    ) where

import qualified Data.List.Split as Split
import qualified Data.List as List

import Data.Either
import Data.String.Utils
import Data.CSV

import Text.ParserCombinators.Parsec

import qualified RoughSetTheory.InfoTable     as InfoTable
import qualified RoughSetTheory.Variant       as Variant
import qualified RoughSetTheory.Approximation as Approximation
import qualified RoughSetTheory.LEM2          as LEM2
import qualified RoughSetTheory.Rule          as Rule
import qualified RoughSetTheory.Reduct        as Reduct
import qualified RoughSetTheory.Classify      as Classify

import Text.Pretty.Simple

file1 = "data/girls.csv"
file2 = "data/wd4.csv"
file3 = "data/monk.csv"
file4 = "data/wwd2.csv"
file5 = "data/wwd11.csv"
file6 = "data/train.csv"
file7 = "data/flu.csv"
file8 = "data/wwd11_red.csv"
file9 = "data/golf.csv"

test1 = "data/girls_test.csv"
test2 = "data/golf_test.csv"
test3 = "data/test.csv"

trainFile = file1
testFile  = test1

approxLevel = Approximation.lowerApprox
-- approxLevel = Approximation.boundary
-- approxLevel = Approximation.upperApprox


loadDataFromCsv :: FilePath -> IO (Either ParseError [[String]])
loadDataFromCsv path = fmap (fmap (fmap strip)) <$> parseFromFile csvFile path


dataExample :: IO ()
dataExample = do
    data_ <- loadDataFromCsv trainFile
    case data_ of
        Left e  -> print e
        Right x -> print $ InfoTable.fromLists x
            
approximationExample :: IO ()
approximationExample = do
    data_ <- loadDataFromCsv trainFile
    case data_ of
        Left e  -> print e
        Right x -> do
            let it = InfoTable.fromLists x
            let approx = Approximation.approximate it
            mapM_ print approx

lem2Example :: IO ()
lem2Example = do
    data_ <- loadDataFromCsv trainFile
    case data_ of
        Left e  -> print e
        Right x -> do
            let it = InfoTable.fromLists x
            let approx = approxLevel <$> Approximation.approximate it
            let allRules = concatMap (LEM2.lem2 (InfoTable.variants it)) approx
            let rules = LEM2.dropUnnecessaryRules allRules (InfoTable.variants it)
            mapM_ print rules

reductExample :: IO ()
reductExample = do
    data_ <- loadDataFromCsv trainFile
    case data_ of
        Left e  -> print e
        Right x -> do
            let it = InfoTable.fromLists x
            let reducts = Reduct.findReducts it 
            let core    = Reduct.core reducts
            putStrLn $ "Reducts: " ++ Reduct.showReducts reducts
            putStrLn $ "Core:    " ++ Reduct.showCore (Reduct.core reducts)

classifyExample :: IO ()
classifyExample = do
    data_ <- loadDataFromCsv trainFile
    testSet <- loadDataFromCsv testFile
    case data_ of
        Left e  -> print e
        Right x -> case testSet of
            Left e  -> print e
            Right y -> do
                let it = InfoTable.fromLists x
                let approx = approxLevel <$> Approximation.approximate it
                let allRules = concatMap (LEM2.lem2 (InfoTable.variants it)) approx
                let rules =  LEM2.dropUnnecessaryRules allRules (InfoTable.variants it)
                let test = let it = InfoTable.unclassifiedFromLists y in InfoTable.variants it
                mapM_ print test
                putStrLn ""
                mapM_ print $ concatMap (Classify.classifyVariant rules) test
