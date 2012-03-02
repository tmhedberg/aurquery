import Control.Monad

import Data.Maybe

import System.Console.GetOpt
import System.Environment

import AURQuery.Local
import AURQuery.Remote
import AURQuery.Types

main = do
    (opts, _, _) <-
        fmap
            (getOpt
                Permute
                [(Option ['d']
                         ["aurdir"]
                         (ReqArg id "DIR")
                         "Directory where you extract AUR tarballs")])
            getArgs
    ipkgs <- installedPkgs $ listToMaybe opts
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
