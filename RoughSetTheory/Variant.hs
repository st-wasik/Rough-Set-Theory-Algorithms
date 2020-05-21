module RoughSetTheory.Variant where 

import Data.List

data Variant = Variant
    { name :: String
    , attribs  :: [String]
    , decision :: String
    } deriving (Eq, Ord)

instance Show Variant where
    show io = "{" ++ (let n = name io in if n == "" then "" else n ++ ": ") ++ (intercalate " " $ (fmap (\a -> take 10 $ a ++ (repeat ' ')) $ attribs io)) ++ " => " ++ (decision io) ++ "}"
    --show io = "{" ++ name io ++ "}"
    
fromList :: [String] -> Variant
fromList input = Variant "" (init input) (last input)

fromListWithName :: [String] -> Variant
fromListWithName input = Variant (head input) (init . tail $ input) (last input)