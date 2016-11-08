{-# LANGUAGE OverloadedStrings #-}

module Challenge4 () where

import qualified Data.Text.Lazy as T
import System.IO
import Challenge3

main = do
    h <- openFile "4.txt" ReadMode
    c <- hGetContents h
    mapM_ print $ (take 1 . filter (not . T.null) . map (findXor 2) . T.lines . T.pack) c