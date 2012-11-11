module AURQuery.Remote (remotePkg) where

import Control.Monad.Trans.Resource

import Data.ByteString.Lazy.UTF8 hiding (take)

import Network.HTTP.Conduit
import Network.HTTP.Types.Status

import AURQuery.Pkgbuild
import AURQuery.Types hiding (version)

pkgbuildURI :: String -> String
pkgbuildURI pkg = "https://aur.archlinux.org/packages/"
               ++ take 2 pkg
               ++ "/"
               ++ pkg
               ++ "/PKGBUILD"

pkgbuild :: Manager -> String -> IO (Maybe String)
pkgbuild mgr pkg = runResourceT $ do
    req <- parseUrl $ pkgbuildURI pkg
    resp <- httpLbs req {checkStatus = \_ _ -> Nothing} mgr
    case responseStatus resp of
        Status 200 _ -> return . Just $ toString $ responseBody resp
        _ -> return Nothing

remotePkg :: Manager -> String -> IO (Maybe Package)
remotePkg mgr pkg = do
    mpb <- pkgbuild mgr pkg
    return $ mpb >>= \v ->
        if null v then Nothing else parseVer (version v) >>= Just . Pkg pkg
