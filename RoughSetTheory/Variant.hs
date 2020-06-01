module RoughSetTheory.Variant where 

import Data.List

type Condition = (String, String)

data Variant = Variant
    { name :: String
    , attribs  :: [String]
    , attribsNames :: [String]
    , decision :: String
    , decisionName :: String
    } deriving (Eq, Ord)

instance Show Variant where
    show v = 
        "{" 
        ++ (let n = name v in if n == "" then "" else n ++ ": ") 
        ++ (unwords . fmap (\(a,b) -> a ++ "=" ++ b) $ toAttrConditions v)
        ++ " => " 
        ++ decision v 
        ++ "}"

fromList :: [String] -> [String] -> Variant
fromList atrNames input = Variant "" (init input) (init atrNames) (last input) (last atrNames)

fromListWithName :: [String] -> [String] -> Variant
fromListWithName atrNames input = Variant (head input) (init . tail $ input) (init . tail $ atrNames) (last input) (last atrNames)

toAttrConditions :: Variant -> [Condition]
toAttrConditions v = zip (attribsNames v) (attribs v)

toDecCondition :: Variant -> Condition
toDecCondition v = (decisionName v, decision v)

toConditions :: Variant -> [Condition]
toConditions v = toAttrConditions v ++ [toDecCondition v]