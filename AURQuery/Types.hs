module AURQuery.Types where

import Data.Char
import Data.Functor
import Data.Version

import Text.ParserCombinators.ReadP

data Package = Pkg {name :: String, version :: TaggedVersion}

newtype TaggedVersion = TVersion {getVersion :: Version} deriving Eq

instance Ord TaggedVersion where
    TVersion v1 `compare` TVersion v2 =
        case branchComparison of EQ -> versionTags v1 `compare` versionTags v2
                                 _ -> branchComparison
        where branchComparison = versionBranch v1 `compare` versionBranch v2

instance Show TaggedVersion where show = showVersion . getVersion

parseVer :: String -> Maybe TaggedVersion
parseVer s
    | length
        (filter
            (\c ->
                not . or $ ($c) <$> [isAlphaNum, isSpace, flip elem ['.', '-']])
            s)
        > 0 =
            Nothing
    | otherwise = case readP_to_S parseVersion s of
        [] -> Nothing
        ps -> Just $ TVersion . fst $ last ps
