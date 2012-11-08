import Prelude hiding (catch)

import Control.Exception
import Control.Monad

import Data.Function
import Data.Maybe

import System.Console.GetOpt
import System.Console.Terminfo
import System.Environment
import System.Exit
import System.IO

import AURQuery.Local
import AURQuery.Remote
import AURQuery.Types

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

    forM_ ipkgs $ \(Pkg pname lv) -> do
        mrp <- remotePkg pname
        case (mrp, lv) of
            (Nothing, _) -> putStrLn $ pname ++ ": NOT FOUND"
            (Just (Pkg _ (Right rv)), Left lvStr) ->
                when (show rv /= lvStr) $
                    printColor White $
                        pname ++ " (" ++ show lv ++ " -> " ++ show rv ++ ")"
            (Just (Pkg _ (Right rv)), Right lv) ->
                when (rv > lv) $
                    printColor
                        (let gton f = ((>) `on` f) rv lv
                         in if gton getEpoch then Red
                            else if gton majVer then Yellow
                            else if gton branch then Cyan
                            else Green)
                        (pname ++ " (" ++ show lv ++ " -> " ++ show rv ++ ")")
