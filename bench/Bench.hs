{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Criterion.Main
import qualified Data.ByteString            as BS
import           Regex.Rure
import           System.IO.Unsafe           (unsafeDupablePerformIO)
import           Text.Regex.TDFA            (ExecOption (..), defaultCompOpt)
import qualified Text.Regex.TDFA.ByteString as TDFA

main :: IO ()
main =
    defaultMain [ bgroup "rure"
                      [ bench "matches" $ nf (matchesϵ datePtr) "1986-08-28 1977-09-13"
                      ]
                , bgroup "tdfa"
                      [ bench "matches" $ nf (TDFA.execute dateTDFA) "1986-08-28 1977-09-13"
                      ]
                ]
    where datePtr = rureCompile "\\d{4}-\\d{2}-\\d{2}"
          dateTDFA = tdfaCompile "[0-9]{4}-[0-9]{2}-[0-9]{2}"
          tdfaCompile = yeetString . TDFA.compile defaultCompOpt (ExecOption False)
          rureCompile = yeetString . compile'
          yeetString = either (error . show) id

compile' :: BS.ByteString -> Either String RurePtr
compile' re = unsafeDupablePerformIO $ compile rureDefaultFlags re

matchesϵ :: RurePtr -> BS.ByteString -> [RureMatch]
matchesϵ re str = unsafeDupablePerformIO $ matches' re str
