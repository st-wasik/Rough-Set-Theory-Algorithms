module RoughSetTheory.Rule where 

import Data.List

type AttributeValue = (String, String)

data Rule = 
    Rule 
        { conditions :: [AttributeValue]
        , decision :: [AttributeValue]
        } deriving Eq

instance Show Rule where 
    show rule = "IF " ++ conds ++ " THEN " ++ dec 
        where 
            conds = intercalate " AND " $ foldl _concat [] $ conditions rule
            dec = intercalate " OR " $ foldl _concat [] $ decision rule
            _concat acc (a, v) = acc ++ [a ++ "=" ++ v]