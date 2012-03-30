import Prelude hiding (catch)

import Control.Exception
import Control.Monad

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
                (ReqArg id "DIR")
                "Directory where you extract AUR tarballs (default: $HOME/aur)"
          , Option ['h', '?']
                   ["help"]
                   (NoArg "HELP")
                   "Display usage information"
          , Option ['c']
                   ["no-color"]
                   (NoArg "NO_COLOR")
                   "Do not colorize output"
          ]

main = do

    (opts, _, _) <- fmap (getOpt Permute options) getArgs
    when
        ("HELP" `elem` opts)
        (getProgName
            >>= (\pn ->
                    putStr $
                        usageInfo ("Usage: " ++ pn ++ " [<options>]") options)
            >> exitSuccess)

    term <-
        setupTermFromEnv
            `catch` ((const $ setupTerm "dumb")
                         :: SetupTermError -> IO Terminal)
    isatty <- hIsTerminalDevice stdout
    let printColor c =
            case ( getCapability term withForegroundColor
                 , "NO_COLOR" `elem` opts
                 , isatty
                 ) of
                (Just wfg, False, True) ->
                    runTermOutput term . termText . wfg c . (++"\n")
                _ -> putStrLn

    ipkgs <- installedPkgs $ listToMaybe $
        filter (\o -> o /= "HELP" && o /= "NO_COLOR") opts
    forM_ ipkgs $ \(Pkg pname lv) -> do
        mrp <- remotePkg pname
        case mrp of
            Just (Pkg _ rv) ->
                when (rv > lv) $
                    printColor
                        (if getEpoch rv > getEpoch lv then Red
                         else if majVer rv > majVer lv then Yellow
                         else if branch rv > branch lv then Blue
                         else Green)
                        (pname ++ " (" ++ show lv ++ " -> " ++ show rv ++ ")")
            Nothing -> putStrLn $ pname ++ ": NOT FOUND"
