{-# LANGUAGE OverloadedStrings #-}

module Challenge5 (repeatingKeyXor, stoh) where

import Data.Char
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as B 
import Challenge1
import Challenge2
import Challenge3

main = print $ r1 == repeatingKeyXor line1 key

repeatingKeyXor :: B.ByteString -> B.ByteString -> B.ByteString
repeatingKeyXor msg key = B.concat . map (\(a, b) -> toh (ord a) (ord b)) . B.zip msg $ rep
    where rep = B.take (B.length msg) . B.cycle $ key
          toh a b = 
            let h = itoh . toInteger . xor a $ b
            in if B.length h == 1 then '0' `B.cons` h else h
    
stoh :: B.ByteString -> B.ByteString
stoh = B.concat . B.foldr (\c a -> (itoh . toInteger . ord $ c) : a) []

key = "ICE"
line1 = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
r1 = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a2622632427276527\
  \2a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"