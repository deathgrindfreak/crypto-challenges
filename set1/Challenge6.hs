{-# LANGUAGE OverloadedStrings #-}

module Challenge6 where

import System.IO
import Data.Char
import Data.Bits
import Data.List
import Data.Ord
import qualified Data.ByteString.Lazy.Char8 as B

import Challenge1
import Challenge3
import Challenge5

main = do
    --h <- openFile "6.txt" ReadMode
    --c <- filter (`notElem` "=\n") `fmap` hGetContents h
    let --cs = b64toh . T.pack $ c
        cs = r1
        --ks = map (*2) . keySizes $ cs
        ks = [3]
        ch = map (\k -> transform . filter (\x -> (fromIntegral . B.length $ x) == k) . chunksOf k $ cs) ks
    --print ch
    print $ keySz cs
    --print ks
    --print $ map (\k -> chunksOf k cs) ks
    --print ch
    --mapM_ (print . map (findXor 1)) ch 
    
keySz msg = sortBy (comparing snd) . zip [2..40] . map keyDist $ [2..40]
    where keyDist k = 
            let k' = 2 * k
                d = hamming (B.take k' msg) (B.take k' . B.drop k' $ msg)
            in (fromIntegral d) / (fromIntegral k)

-- Find the top 3 most likely key sizes
keySizes :: B.ByteString -> [Int]
keySizes msg = map fst . take 3 . sortBy (comparing snd) . zip [2..40] . map keyDist $ [2..40]
    where keyDist k = 
            let k' = 2 * k
                d = hamming (B.take k' msg) (B.take k' . B.drop k' $ msg)
            in (fromIntegral d) / (fromIntegral k)

-- Hamming distance between two strings
hamming :: B.ByteString -> B.ByteString -> Int
hamming s t = popCount . xor (htoi s) $ (htoi t)
    
transform :: [B.ByteString] -> [B.ByteString]
transform lst = foldr (\l a -> zipWith (B.append) (chunksOf 2 l) a) emptyLst lst
    where emptyLst = (replicate (fromIntegral . B.length . head $ lst) "")
    
chunksOf :: Int -> B.ByteString -> [B.ByteString]
chunksOf _ "" = []
chunksOf n xs = (B.take (fromIntegral n) xs) : chunksOf n (B.drop (fromIntegral n) xs)

key = "ICE"
line1 = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
r1 = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527\
  \2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"