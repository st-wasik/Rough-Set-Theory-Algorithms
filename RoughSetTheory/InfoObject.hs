module RoughSetTheory.InfoObject where 

import Data.List

data InfoObject = InfoObject
    { attribs  :: [String]
    , decision :: String
    } deriving (Eq, Ord)

instance Show InfoObject where
    show io = (intercalate "\t" $ attribs io) ++ " \t=> " ++ (decision io) ++ ""

fromList :: [String] -> InfoObject
fromList input = InfoObject (init input) (last input)
