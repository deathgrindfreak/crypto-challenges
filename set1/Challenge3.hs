{-# LANGUAGE OverloadedStrings #-}

module Challenge3 (htos, findXor, singleXor) where

import Data.Char
import Data.List
import Data.Ord
import qualified Data.ByteString.Lazy.Char8 as B 
import Challenge1
import Challenge2

import IPPrint

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

main = do
    let x = findXor' message 
    pprint $ sortBy (comparing findE) x
    where findE l 
            = case lookup 'e' (fst l) of
                Just e -> e
                Nothing -> 0
        
    

--findXor' :: Int -> T.Text -> T.Text
findXor' msg = filter (not . null . fst) . zip hist $ decrypts
    where 
    decrypts = map (B.unpack . singleXor msg) [0..255] 
    hist = map (\d -> map (\g -> (head g, length g)) . group . sort . map toLower 
        . filter (`elem` (freqLst ++ map toUpper freqLst)) $ d) decrypts
    freqLst = "etaoinshrdlu"

findXor :: Int -> B.ByteString -> B.ByteString
findXor k msg = (snd . sHead . sortBy (comparing snd) . filter (\(f, w) -> f > k)) frequencies
    where 
    decrypts = map (singleXor msg) [0..255] 
    frequencies = map (\d -> (frequency (B.words . B.map toLower $ d), d)) decrypts
    frequency d = foldr (\w a ->  a + (length . filter (w ==) $ d)) 0 commonWords
    sHead l = if null l then (0, "") else head l

singleXor :: B.ByteString -> Integer -> B.ByteString
singleXor m x = htos $ xorHex m cx 
    where x' = itoh x
          cx = B.take (B.length m) (B.cycle (if B.length x' == 1 then '0' `B.cons` x' else x'))

htos :: B.ByteString -> B.ByteString
htos "0" = "0"
htos s = tos (htoi s) ""
    where
    tos 0 s = s
    tos x s = tos q (chr (fromInteger r) `B.cons` s)
        where (q, r) = x `divMod` 256