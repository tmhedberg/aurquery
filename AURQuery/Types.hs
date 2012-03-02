module AURQuery.Types where

import Data.Version

import Text.ParserCombinators.ReadP

data Package = Pkg {name :: String, version :: Version}

parseVer :: String -> Version
parseVer s = fst . last $ readP_to_S parseVersion s
