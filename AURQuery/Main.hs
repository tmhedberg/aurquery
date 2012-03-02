import AURQuery.Local
import AURQuery.Remote

import Control.Monad

import Data.Version

main = do
    ipkgs <- installedPkgs
    forM_ ipkgs $ \(Pkg pname lv) -> do
        mrp <- remotePkg pname
        putStrLn $ pname ++ ": " ++ case mrp of
            Just (Pkg _ rv) -> showVersion lv ++ " / " ++ showVersion rv
            Nothing -> "NOT FOUND"
