module RoughSetTheory.InfoObject where 

import Data.List

data InfoObject = InfoObject
    { name :: String
    , attribs  :: [String]
    , decision :: String
    } deriving (Eq, Ord)

instance Show InfoObject where
    show io = "{" ++ (let n = name io in if n == "" then "" else n ++ " : ") ++ (intercalate " " $ attribs io) ++ " => " ++ (decision io) ++ "}"
    --show io = "{" ++ name io ++ "}"
    
fromList :: [String] -> InfoObject
fromList input = InfoObject "" (init input) (last input)

fromListWithName :: [String] -> InfoObject
fromListWithName input = InfoObject (head input) (init . tail $ input) (last input)