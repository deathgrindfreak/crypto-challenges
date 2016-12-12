{-# LANGUAGE OverloadedStrings #-}

module Challenge4 () where

import qualified Data.ByteString.Lazy.Char8 as B 
import System.IO
import Challenge3

main = do
    h <- openFile "4.txt" ReadMode
    c <- hGetContents h
    mapM_ print $ (take 1 . filter (not . B.null) . map (findXor 2) . B.lines . B.pack) c