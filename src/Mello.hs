{- |
Copyright: (c) 2021 Tito Sacchi
SPDX-License-Identifier: GPL-3.0-only
Maintainer: Tito Sacchi <tito.sakki@gmail.com>

Haskell memory inspection, debugging and tracing library
-}

module Mello
       ( someFunc
       ) where

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
