module AURQuery.Local (installedPkgs) where

import Control.Monad

import Data.List
import Data.Maybe

import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

import AURQuery.Types

aurDirBasename = "aur"

aurDir :: Maybe FilePath -> IO FilePath
aurDir = maybe (fmap (</>aurDirBasename) (getEnv "HOME")) return

aurDirs :: Maybe FilePath -> IO [FilePath]
aurDirs base = do
    ad <- aurDir base
    fmap sort $ getDirectoryContents ad
            >>= filterM (doesDirectoryExist . (ad</>))
            >>= filterM (return . not . isPrefixOf ".")

installedPkgs :: Maybe FilePath -> IO [Package]
installedPkgs base = do
    ads <- aurDirs base
    vers <- forM ads $ \pname -> do
        devNull <- openFile "/dev/null" WriteMode
        (_, Just out, _, ph) <- createProcess
            (proc "/usr/bin/pacman" ["-Qi", pname])
                {std_out = CreatePipe, std_err = UseHandle devNull}
        _ <- waitForProcess ph
        hClose devNull
        hGetContents out
    mpkgs <- forM (filter (not . null . snd) (zip ads vers))
                $ \(pname, pacOutput) -> do
                    let (':':' ':pver) = dropWhile (/=':')
                                       . head
                                       . filter (isPrefixOf "Version")
                                       . lines
                                       $ pacOutput
                    case parseVer pver of
                        Nothing ->
                            hPutStrLn
                                stderr
                                ("Unable to parse version: "
                                    ++ pname
                                    ++ " ("
                                    ++ pver
                                    ++ ")") >>
                            return Nothing
                        Just v -> return . Just $ Pkg pname v
    return $ map fromJust $ filter isJust mpkgs
