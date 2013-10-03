#!/usr/bin/env runhaskell
> module Main (main) where
>
> import           System.Process
> import           System.Exit
>
> main :: IO ()
> main = rawSystem "cabal" ["install", "hspec"] >> rawSystem "runhaskell" ["-itest", "-isrc", "-optP-include", "-optPdist/build/autogen/cabal_macros.h", "test/Spec.hs"] >>= exitWith
