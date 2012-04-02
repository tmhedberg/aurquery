module AURQuery.Types ( Package (..)
                      , TaggedVersion (..)
                      , branch
                      , majVer
                      , parseVer
                      ) where

import Data.Char
import Data.Monoid
import Data.Ord
import Data.Version

import Text.ParserCombinators.ReadP

data Package = Pkg {name :: String, version :: TaggedVersion}

instance Show Package where show p = name p ++ " (" ++ show (version p) ++ ")"

data TaggedVersion = TVersion {getEpoch :: Int, getVersion :: Version}
    deriving Eq

instance Ord TaggedVersion where
    compare = comparing getEpoch
           <> comparing getVersion
           <> comparing (versionTags . getVersion)

instance Show TaggedVersion where
    show (TVersion e v) = (if e /= 0 then show e ++ ":" else "")
                       ++ showVersion v

parseVer :: String -> Maybe TaggedVersion
parseVer s = case readP_to_S parseTVersion s of
    [] -> Nothing
    ps -> let v = fst $ last ps in if show v == s then Just v else Nothing

parseEpoch :: ReadP Int
parseEpoch = munch1 isDigit >>= \epoch -> char ':' >> return (read epoch)

parseTVersion :: ReadP TaggedVersion
parseTVersion = do epoch <- option 0 parseEpoch
                   vers <- parseVersion
                   return $ TVersion epoch vers

branch :: TaggedVersion -> [Int]
branch = versionBranch . getVersion

majVer :: TaggedVersion -> Int
majVer = head . branch
