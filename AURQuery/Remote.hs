{-# LANGUAGE ViewPatterns #-}

module AURQuery.Remote (remotePkg) where

import Network.HTTP
import Network.URI

import AURQuery.Pkgbuild
import AURQuery.Types hiding (version)

pkgbuildURI :: String -> URI
pkgbuildURI pkg =
    URI "http:"
        (Just $ URIAuth "" "aur.archlinux.org" "")
        ("/packages/" ++ take 2 pkg ++ "/" ++ pkg ++ "/PKGBUILD")
        ""
        ""

pkgbuild :: String -> IO (Maybe String)
pkgbuild pkg = do
    resp <- simpleHTTP $ Request (pkgbuildURI pkg) GET [] ""
    case resp of Right r@(rspCode -> (2, 0, 0)) -> return . Just $ rspBody r
                 _ -> return Nothing

remotePkg :: String -> IO (Maybe Package)
remotePkg pkg = do
    mpb <- pkgbuild pkg
    return $ mpb >>= \v ->
        if null v then Nothing else parseVer (version v) >>= Just . Pkg pkg
