{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Main
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Main (main)
  where

import System.IO (IO)

--import Test.Tasty (TestTree, defaultMainWithIngredients, listingTests, testGroup)
import Test.Tasty (TestTree, defaultMain, testGroup)
--import Test.Tasty.Runners.JenkinsXML (jenkinsXMLRunner)

import qualified Test.Data.HeytingAlgebra.Class as Class (tests)


main :: IO ()
--main = defaultMainWithIngredients ingredients tests
main = defaultMain tests
  where
--  ingredients = [listingTests, jenkinsXMLRunner]

    tests :: TestTree
    tests = testGroup "Test"
      [ Class.tests
      ]
