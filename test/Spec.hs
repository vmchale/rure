{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString  as BS
import           Regex.Rure
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "High-level interface"
        [ testCase "Compiles a regex" (compileY ".*")
        ]

compileY :: BS.ByteString -> Assertion
compileY bs = do
    res <- compile bs
    case res of
        Left err -> assertFailure err
        Right{}  -> assertBool "success." True
