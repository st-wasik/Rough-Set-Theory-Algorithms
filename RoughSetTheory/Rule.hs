module RoughSetTheory.Rule where 

import Data.List

type AttributeValue = (String, String)

data Rule = 
    Rule 
        { conditions :: [AttributeValue]
        , decision :: [AttributeValue]
        } deriving Eq

instance Show Rule where 
    show rule = "if " ++ conds ++ " then " ++ dec 
        where 
            conds = intercalate " and " $ foldl _concat [] $ conditions rule
            dec = intercalate " or " $ foldl _concat [] $ decision rule
            _concat acc (a, v) = acc ++ [a ++ "=" ++ v]