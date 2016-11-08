{-# LANGUAGE OverloadedStrings #-}

module Challenge6 where

import System.IO
import Data.Char
import Data.Bits
import Data.List
import Data.Ord
import qualified Data.Text.Lazy as T

import Challenge1
import Challenge3
import Challenge5

main = do
    h <- openFile "6.txt" ReadMode
    c <- filter (`notElem` "=\n") `fmap` hGetContents h
    let cs = b64toh . T.pack $ c
        ks = keySizes cs
        ch = map (\k -> transform . filter (\x -> (fromIntegral . T.length $ x) == k) . chunksOf k $ cs) ks
    print $ map (\c -> map (findXor 1) c) ch

-- Find the top 3 most likely key sizes
keySizes :: T.Text -> [Int]
keySizes msg = map fst . take 3 . sortBy (comparing snd) . zip [2..40] . map keyDist $ [2..40]
    where keyDist k = 
            let d = hamming (T.take k msg) (T.take k . T.drop k $ msg)
            in (fromIntegral d) / (fromIntegral k)

-- Hamming distance between two strings
hamming :: T.Text -> T.Text -> Int
hamming s t = popCount . xor (stoi s) $ (stoi t)
    where stoi = htoi . stoh
    
transform :: [T.Text] -> [T.Text]
transform lst = foldr (\l a -> zipWith (T.cons) (T.unpack l) a) (replicate (fromIntegral . T.length . head $ lst) "") lst
    
chunksOf :: Int -> T.Text -> [T.Text]
chunksOf _ "" = []
chunksOf n xs = (T.take (fromIntegral n) xs) : chunksOf n (T.drop (fromIntegral n) xs)