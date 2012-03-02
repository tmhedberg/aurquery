module Remote (pkgbuild) where

import Network.HTTP
import Network.URI

pkgbuildURI :: String -> URI
pkgbuildURI pkg =
    URI "http:"
        (Just $ URIAuth "" "aur.archlinux.org" "")
        ("/packages/" ++ take 2 pkg ++ "/" ++ pkg ++ "/PKGBUILD")
        ""
        ""

pkgbuild :: String -> IO (Maybe String)
pkgbuild pkg = do resp <- simpleHTTP $ Request (pkgbuildURI pkg) GET [] ""
                  case resp of Right r -> return . Just $ rspBody r
                               Left _ -> return Nothing
