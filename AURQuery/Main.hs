{-# LANGUAGE MultiWayIf #-}

import Prelude

import Control.Exception
import Control.Monad

import Data.Default
import Data.Function
import Data.List
import Data.Maybe

import Network.HTTP.Conduit

import System.Console.GetOpt
import System.Console.Terminfo
import System.Environment
import System.Exit
import System.IO

import AURQuery.Local
import AURQuery.Remote
import AURQuery.Types

vcsSuffixes = ["cvs", "svn", "git", "hg", "bzr", "darcs"]

options = [ Option
                ['d']
                ["aurdir"]
                (ReqArg AURDir "DIR")
                "Directory where you extract AUR tarballs (default: $HOME/aur)"
          , Option ['h', '?'] ["help"] (NoArg Help) "Display usage information"
          , Option ['c'] ["no-color"] (NoArg NoColor) "Do not colorize output"
          , Option ['l']
                   ["list-installed"]
                   (NoArg ListInstalled)
                   "List installed AUR packages"
          ]

data Option = AURDir String | Help | NoColor | ListInstalled deriving Eq

getAURDirOpt :: [Option] -> Maybe String
getAURDirOpt = listToMaybe . catMaybes . map (\o -> case o of AURDir s -> Just s
                                                              _ -> Nothing)

main = do

    (opts, _, _) <- fmap (getOpt Permute options) getArgs

    when (Help `elem` opts) $
        getProgName
            >>= (\pn ->
                    putStr $
                        usageInfo ("Usage: " ++ pn ++ " [<options>]") options)
            >> exitSuccess

    ipkgs <- installedPkgs $ getAURDirOpt opts
    when (ListInstalled `elem` opts) (mapM print ipkgs >> exitSuccess)

    term <-
        setupTermFromEnv
            `catch` ((const $ setupTerm "dumb")
                         :: SetupTermError -> IO Terminal)
    isatty <- hIsTerminalDevice stdout
    let printColor c =
            case ( getCapability term withForegroundColor
                 , NoColor `elem` opts
                 , isatty
                 ) of
                (Just wfg, False, True) ->
                    runTermOutput term . termText . wfg c . (++"\n")
                _ -> putStrLn

    bracket (newManager def) closeManager $ \httpMgr ->
        forM_ ipkgs $ \(Pkg pname e_lv) -> do

            let printPkgVersionChange color localVer remoteVer =
                    when (localVer /= remoteVer && not (isVCSPackage pname)) $
                        printColor color $ pname
                                        ++ " ("
                                        ++ localVer
                                        ++ " -> "
                                        ++ remoteVer
                                        ++ ")"

            mrp <- remotePkg httpMgr pname

            case (mrp, e_lv) of
                (Nothing, _) -> putStrLn $ pname ++ ": NOT FOUND"
                (Just (Pkg _ (Left rvStr)), Left lvStr) ->
                    printPkgVersionChange White lvStr rvStr
                (Just (Pkg _ (Right rv)), Left lvStr) ->
                    printPkgVersionChange White lvStr $ show rv
                (Just (Pkg _ (Left rvStr)), Right lv) ->
                    printPkgVersionChange White (show lv) rvStr
                (Just (Pkg _ (Right rv)), Right lv) ->
                    printPkgVersionChange (verDeltaColor lv rv)
                                          (show lv)
                                          (show rv)

verDeltaColor :: TaggedVersion -> TaggedVersion -> Color
verDeltaColor lv rv = let gton f = ((>) `on` f) rv lv
                      in if | gton getEpoch -> Red
                            | gton majVer -> Yellow
                            | gton branch -> Cyan
                            | otherwise -> Green

isVCSPackage :: String -> Bool
isVCSPackage pname = any (flip isSuffixOf pname . ('-':)) vcsSuffixes
