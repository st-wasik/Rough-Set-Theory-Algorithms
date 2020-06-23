module RoughSetTheory.Rule where 

import Data.List

data Rule = 
    Rule 
        { conditions :: [(String, String)]
        , decision :: [(String, String)]
        } deriving Eq

instance Show Rule where 
    show rule = "IF " ++ conds ++ " THEN " ++ dec 
        where 
            conds = intercalate " AND " $ foldl _concat [] $ conditions rule
            dec = intercalate " OR " $ foldl _concat [] $ decision rule
            _concat acc (a, v) = acc ++ [a ++ "=" ++ v]

decisionAttribName :: Rule -> String
decisionAttribName r = let ds = decision r in if null ds then "" else fst $ head ds
