module RoughSetTheory.InfoObject where 

import Data.List

data InfoObject = InfoObject
    { attribs  :: [String]
    , decision :: String
    } 

instance Show InfoObject where
    show io = (intercalate " " $ attribs io) ++ " => " ++ (decision io) ++ ""

fromList :: [String] -> InfoObject
fromList input = InfoObject (init input) (last input)