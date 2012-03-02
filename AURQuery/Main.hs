import Control.Monad

import Data.Maybe

import System.Console.GetOpt
import System.Environment
import System.Exit

import AURQuery.Local
import AURQuery.Remote
import AURQuery.Types

options = [ (Option
                ['d']
                ["aurdir"]
                (ReqArg id "DIR")
                "Directory where you extract AUR tarballs (default: $HOME/aur)")
          , (Option ['h', '?']
                    ["help"]
                    (NoArg "HELP")
                    "Display usage information")
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
    ipkgs <- installedPkgs $ listToMaybe $ filter (/="HELP") opts
    forM_ ipkgs $ \(Pkg pname lv) -> do
        mrp <- remotePkg pname
        case mrp of
            Just (Pkg _ rv) ->
                when (rv > lv)
                     (putStrLn $ pname
                              ++ " ("
                              ++ show lv
                              ++ " -> "
                              ++ show rv
                              ++ ")")
            Nothing -> putStrLn $ pname ++ ": NOT FOUND"
