module Main
  ( main
  ) where

import Test.Hspec (hspec)

import qualified FileManagerSpec as FMS
--
main :: IO ()
main = hspec $ do
  FMS.fullSpec
