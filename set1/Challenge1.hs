{-# LANGUAGE OverloadedStrings #-}

module Challenge1 (hto64, ito64, htoi, itoh) where

import qualified Data.Text.Lazy as T
import Data.Char

main = putStrLn $ "Challenge1: " ++ show (test64 == hto64 testHex)

hto64 :: T.Text -> T.Text
hto64 = ito64 . htoi

itoh :: Integer -> T.Text
itoh 0 = "0"
itoh i = toh i ""
    where 
    toh 0 s = s
    toh x s = toh (x `div` 16) ((ash . fromInteger . mod x $ 16) `T.cons` s)
    
    ash x | 0 <= x && x <= 9 = chr (x + ord '0')
          | 10 <= x && x <= 15 = ['a'..'f'] !! (x - 10)

ito64 :: Integer -> T.Text
ito64 0 = "A"
ito64 i = to64 i ""
    where 
    to64 0 s = s
    to64 x s = to64 (x `div` 64) ((as64 . fromInteger . mod x $ 64) `T.cons` s)
    
    as64 62 = '+'
    as64 63 = '/'
    as64 x | 0 <= x && x <= 25 = ['A'..'Z'] !! x
           | 25 < x && x <= 51 = ['a'..'z'] !! (x - 26)
           | 51 < x && x <= 61 = ['0'..'9'] !! (x - 52)

htoi :: T.Text -> Integer
htoi = T.foldl (\a x -> 16 * a + (toInteger . hex $ x)) 0
    where 
    hex x | x `elem` ['a'..'f'] = ord x - ord 'a' + 10
          | otherwise = ord x - ord '0'

testHex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
test64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"