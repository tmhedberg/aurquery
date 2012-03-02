{-# LANGUAGE TupleSections #-}

module Local (Package (..), installedPkgs) where

import Control.Monad

import Data.List

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

aurDirBasename = "aur"

data Package = Pkg {name :: String, version :: String}

aurDir :: IO FilePath
aurDir = fmap (</>aurDirBasename) (getEnv "HOME")

aurDirs :: IO [FilePath]
aurDirs = do ad <- aurDir
             fmap sort $ getDirectoryContents ad
                     >>= filterM (doesDirectoryExist . (ad</>))
                     >>= filterM (return . not . isPrefixOf ".")

installedPkgs :: IO [Package]
installedPkgs = do
    ads <- aurDirs
    vers <- withFile "/dev/null" WriteMode $ \devNull -> 
        forM ads $ \pname -> do
            (_, Just out, _, _) <- createProcess
                (proc "/usr/bin/pacman" ["-Qi", pname])
                    {std_out = CreatePipe, std_err = CreatePipe}
            hGetContents out
    forM (filter (not . null . snd) (zip ads vers)) $ \(pname, pacOutput) -> do
        let (':':' ':pver) = dropWhile (/=':')
                           . head
                           . filter (isPrefixOf "Version")
                           . lines
                           $ pacOutput
        return $ Pkg pname pver
