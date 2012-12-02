import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Writer

import Data.Default
import Data.Function
import Data.List
import Data.Maybe

import Network.HTTP.Conduit

import System.Console.GetOpt
import System.Console.Terminfo hiding (row)
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
          , Option
                ['o']
                ["only-parsable-versions"]
                (NoArg OnlyParsableVersions)
                "Do not display packages whose version strings cannot be parsed"
          , Option ['l']
                   ["list-installed"]
                   (NoArg ListInstalled)
                   "List installed AUR packages"
          ]

data Option = AURDir String
            | Help
            | NoColor
            | OnlyParsableVersions
            | ListInstalled
    deriving Eq

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

    putStrLn "Obtaining local package info..."
    ipkgs <- installedPkgs $ getAURDirOpt opts

    let displayColumns (Pkg name_ ver) = [name_, either id show ver]
    when (ListInstalled `elem` opts) $
        mapM_ putStrLn
              (tabulate $ ["Package", "Version"] : map displayColumns ipkgs)
            >> exitSuccess

    term <-
        setupTermFromEnv
            `catch` ((const $ setupTerm "dumb")
                         :: SetupTermError -> IO Terminal)
    isatty <- hIsTerminalDevice stdout
    let printLineSpec (s, c) =
            case ( getCapability term withForegroundColor
                 , NoColor `elem` opts
                 , isatty
                 ) of
                (Just wfg, False, True) ->
                    runTermOutput term . termText . wfg c $ s ++ "\n"
                _ -> putStrLn s

    let whenPrintUnparseable = unless $ OnlyParsableVersions `elem` opts
    bracket (newManager def) closeManager $ \httpMgr -> do
        putStrLn "Downloading AUR package info..."
        lineSpecs <- execWriterT $ forM ipkgs $ \(Pkg pname e_lv) -> do

            let printPkgVersionChange color localVer remoteVer =
                    when (localVer /= remoteVer && not (isVCSPackage pname)) $
                        tell [([pname, localVer, remoteVer], color)]

            mrp <- liftIO $ remotePkg httpMgr pname

            case (mrp, e_lv) of
                (Nothing, _) -> liftIO $ putStrLn $ pname ++ ": NOT FOUND"
                (Just (Pkg _ (Left rvStr)), Left lvStr) ->
                    whenPrintUnparseable $
                        printPkgVersionChange White lvStr rvStr
                (Just (Pkg _ (Right rv)), Left lvStr) -> whenPrintUnparseable $
                    printPkgVersionChange White lvStr $ show rv
                (Just (Pkg _ (Left rvStr)), Right lv) -> whenPrintUnparseable $
                    printPkgVersionChange White (show lv) rvStr
                (Just (Pkg _ (Right rv)), Right lv) ->
                    printPkgVersionChange (verDeltaColor lv rv)
                                          (show lv)
                                          (show rv)

        mapM_ printLineSpec $ uncurry zip $ first tabulate $
            unzip ((["Package", "Old Version", "New Version"], White):lineSpecs)

verDeltaColor :: TaggedVersion -> TaggedVersion -> Color
verDeltaColor lv rv | gton getEpoch = Red
                    | gton majVer = Yellow
                    | gton branch = Cyan
                    | otherwise = Green
    where gton f = ((>) `on` f) rv lv

isVCSPackage :: String -> Bool
isVCSPackage pname = any (flip isSuffixOf pname . ('-':)) vcsSuffixes

tabulate :: [[String]] -> [String]
tabulate rows = map (padColumns columnWidths) equalLengthRows
    where columnWidths = collapseMax $ map (map length) equalLengthRows
          collapseMax = foldl (zipWith max) (repeat 0)
          padColumns ws = intercalate "  " . zipWith pad ws
          pad w s = s ++ replicate (w - length s) ' '
          equalLengthRows = map (extendRow $ maximum $ map length rows) rows
          extendRow len row = row ++ replicate (len - length row) ""
