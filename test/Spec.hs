{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString  as BS
import           Regex.Rure
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "High-level interface"
        [ testCase "Matches a file extension" (matchY "\\.awk$" "/usr/share/vim/vim82/tools/mve.awk")
        , testCase "Matches at" (findAt "libj\\.dylib$" "/Applications/j903/bin/libj.dylib" (RureMatch 23 33))
        , testCase "Matches at (plural)" $
            matchesAt "\\d{4}-\\d{2}-\\d{2}" "2012-03-14 2013-01-01 2014-07-05"
                [ RureMatch 0 10
                , RureMatch 11 21
                , RureMatch 22 32
                ]
        ]

matchesAt :: BS.ByteString
          -> BS.ByteString
          -> [RureMatch]
          -> Assertion
matchesAt re haystack expected =
    let (Right actual) = hsMatches re haystack
        in actual @?= expected

findAt :: BS.ByteString
       -> BS.ByteString
       -> RureMatch
       -> Assertion
findAt re haystack expected =
    let (Right actual) = hsFind re haystack
        in actual @?= Just expected

matchY :: BS.ByteString
       -> BS.ByteString
       -> Assertion
matchY re haystack =
    case hsIsMatch re haystack of
        Left err -> assertFailure err
        Right b  -> assertBool "matches" b
