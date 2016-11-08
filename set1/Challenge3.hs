{-# LANGUAGE OverloadedStrings #-}

module Challenge3 (htos, findXor) where

import Data.Char
import Data.List
import Data.Ord
import qualified Data.Text.Lazy as T
import Challenge1
import Challenge2

-- The 100 most common english words
commonWords = ["the", "be", "to", "of", "and", "a", "in", "that", "have", "i", "it",
    "for", "not", "on", "with", "he", "as", "you", "do", "at", "this", "but", "his",
    "by", "from", "they", "we", "say", "her", "she", "or", "an", "will", "my", "one",
    "all", "would", "there", "their", "what", "so", "up", "out", "if", "about", "who",
    "get", "which", "go", "me", "when", "make", "can", "like", "time", "no", "just", "him",
    "know", "take", "people", "into", "year", "your", "good", "some", "could", "them", "see",
    "other", "than", "then", "now", "look", "only", "come", "its", "over", "think", "also", "back",
    "after", "use", "two", "how", "our", "work", "first", "well", "way", "even", "new", "want",
    "because", "any", "these", "give", "day", "most", "us"]

message = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

main = print $ findXor 1 message 

findXor :: Int -> T.Text -> T.Text
findXor k msg = (snd . sHead . sortBy (comparing snd) . filter (\(f, w) -> f > k)) frequencies
    where 
    decrypts = map (singleXor msg) [0..255] 
    frequencies = map (\d -> (frequency (T.words . T.map toLower $ d), d)) decrypts
    frequency d = foldr (\w a ->  a + (length . filter (w ==) $ d)) 0 commonWords
    sHead l = if null l then (0, "") else head l

singleXor :: T.Text -> Integer -> T.Text
singleXor m x = htos $ xorHex m cx 
    where x' = itoh x
          cx = T.take (T.length m) (T.cycle (if T.length x' == 1 then '0' `T.cons` x' else x'))

htos :: T.Text -> T.Text
htos "0" = "0"
htos s = tos (htoi s) ""
    where
    tos 0 s = s
    tos x s = tos q (chr (fromInteger r) `T.cons` s)
        where (q, r) = x `divMod` 256