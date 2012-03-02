import Control.Monad

import Data.Version

import AURQuery.Local
import AURQuery.Remote
import AURQuery.Types

main = do
    ipkgs <- installedPkgs
    forM_ ipkgs $ \(Pkg pname lv) -> do
        mrp <- remotePkg pname
        case mrp of
            Just (Pkg _ rv) ->
                when (rv > lv)
                     (putStrLn $ pname
                              ++ " ("
                              ++ showVersion (getVersion lv)
                              ++ " -> "
                              ++ showVersion (getVersion rv)
                              ++ ")")
            Nothing -> putStrLn $ pname ++ ": NOT FOUND"
