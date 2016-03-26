module Network.Docker.Utils where

import qualified Data.List as List

stripUnderscore :: String -> String
stripUnderscore ('_':xs) = xs
stripUnderscore xs = error $ List.intercalate " "
                     [ "stripUnderscore:"
                     , xs
                     , "does not start with an underscore"
                     ]
