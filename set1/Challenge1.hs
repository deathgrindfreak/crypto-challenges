{-# LANGUAGE OverloadedStrings #-}

module Challenge1 (b64toh, b64toi, hto64, ito64, htoi, itoh) where

import qualified Data.ByteString.Lazy.Char8 as B 
import Data.Char

main = do
    putStrLn $ "Challenge1: " ++ show (test64 == hto64 testHex)
    putStrLn $ "Challenge1: " ++ show (b64toh test64 == testHex)

hto64 :: B.ByteString -> B.ByteString
hto64 = ito64 . htoi

b64toi :: B.ByteString -> Integer
b64toi = B.foldl (\a x -> 64 * a + (toInteger . asi $ x)) 0
    where
    asi '+' = 62
    asi '/' = 63
    asi x | x `elem` ['A'..'Z'] = ord x - ord 'A'
          | x `elem` ['a'..'z'] = ord x - ord 'a' + 26
          | x `elem` ['0'..'9'] = ord x - ord '0' + 52

b64toh :: B.ByteString -> B.ByteString
b64toh = itoh . b64toi

itoh :: Integer -> B.ByteString
itoh 0 = "0"
itoh i = toh i ""
    where 
    toh 0 s = s
    toh x s = toh (x `div` 16) ((ash . fromInteger . mod x $ 16) `B.cons` s)
    
    ash x | 0 <= x && x <= 9 = chr (x + ord '0')
          | 10 <= x && x <= 15 = ['a'..'f'] !! (x - 10)

ito64 :: Integer -> B.ByteString
ito64 0 = "A"
ito64 i = to64 i ""
    where 
    to64 0 s = s
    to64 x s = to64 (x `div` 64) ((as64 . fromInteger . mod x $ 64) `B.cons` s)
    
    as64 62 = '+'
    as64 63 = '/'
    as64 x | 0 <= x && x <= 25 = ['A'..'Z'] !! x
           | 25 < x && x <= 51 = ['a'..'z'] !! (x - 26)
           | 51 < x && x <= 61 = ['0'..'9'] !! (x - 52)

htoi :: B.ByteString -> Integer
htoi = B.foldl (\a x -> 16 * a + (toInteger . hex $ x)) 0
    where 
    hex x | x `elem` ['a'..'f'] = ord x - ord 'a' + 10
          | otherwise = ord x - ord '0'

testHex = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
test64 = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"