module Pkgbuild (version) where

import Data.List

pkgbuildVal :: String -> String -> String
pkgbuildVal var = tail
                . dropWhile (/='=')
                . head
                . filter (isPrefixOf $ var ++ "=")
                . lines

pkgver :: String -> String
pkgver = pkgbuildVal "pkgver"

pkgrel :: String -> String
pkgrel = pkgbuildVal "pkgrel"

version :: String -> String
version pb = pkgver pb ++ "-" ++ pkgrel pb
