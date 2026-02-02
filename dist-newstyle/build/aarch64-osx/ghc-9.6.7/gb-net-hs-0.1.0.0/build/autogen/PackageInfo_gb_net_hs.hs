{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_gb_net_hs (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "gb_net_hs"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Game networking library with bitpacked serialization"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
