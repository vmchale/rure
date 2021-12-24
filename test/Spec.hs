{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString  as BS
import           Regex.Rure
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
    testGroup "High-level interface"
        [ testCase "Compiles a regex" (compileY ".*")
        , testCase "Matches a file extension" (matchY "\\.awk$" "/usr/share/vim/vim82/tools/mve.awk")
        , testCase "Matches at" (findAt "libj\\.dylib$" "/Applications/j903/bin/libj.dylib" (RureMatch 23 33))
        ]

findAt :: BS.ByteString
       -> BS.ByteString
       -> RureMatch
       -> Assertion
findAt re haystack expected = do
    (Right rePtr) <- compile re
    actual <- find rePtr haystack 0
    actual @?= Just expected

matchY :: BS.ByteString
       -> BS.ByteString
       -> Assertion
matchY re haystack =
    assertBool "matches" (matches re haystack)

matches :: BS.ByteString -- ^ Regex
        -> BS.ByteString -- ^ Haystack
        -> Bool
matches re haystack = unsafePerformIO $ do
    (Right rePtr) <- compile re
    isMatch rePtr haystack 0

compileY :: BS.ByteString -> Assertion
compileY bs = do
    res <- compile bs
    case res of
        Left err -> assertFailure err
        Right{}  -> assertBool "success." True
