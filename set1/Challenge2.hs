{-# LANGUAGE OverloadedStrings #-}

module Challenge2 (xorHex) where

import qualified Data.Text.Lazy as T
import Data.Bits
import Challenge1

main = putStrLn $ "Challenge2: " ++ show (result == xorHex test1 test2)

xorHex :: T.Text -> T.Text -> T.Text
xorHex a b = itoh $ xor (htoi a) (htoi b) 

test1 = "1c0111001f010100061a024b53535009181c"
test2 = "686974207468652062756c6c277320657965"
result = "746865206b696420646f6e277420706c6179"