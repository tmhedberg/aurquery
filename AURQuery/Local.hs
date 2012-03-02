module AURQuery.Local (Package (..), installedPkgs) where

import Control.Monad

import Data.List

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

import AURQuery.Types

aurDirBasename = "aur"

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
    vers <- forM ads $ \pname -> do
        devNull <- openFile "/dev/null" WriteMode
        (_, Just out, _, _) <- createProcess
            (proc "/usr/bin/pacman" ["-Qi", pname])
                {std_out = CreatePipe, std_err = UseHandle devNull}
        hClose devNull
        hGetContents out
    forM (filter (not . null . snd) (zip ads vers)) $ \(pname, pacOutput) -> do
        let (':':' ':pver) = dropWhile (/=':')
                           . head
                           . filter (isPrefixOf "Version") . lines
                           $ pacOutput
        return $ Pkg pname $ parseVer pver
