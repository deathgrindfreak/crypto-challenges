{-# LANGUAGE OverloadedStrings #-}

module Challenge7 () where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV)
import Crypto.Error (CryptoFailable(..))

import qualified Data.ByteArray as A
import Data.ByteString.Lazy.Char8 as B
import Data.ByteString as BS
import Data.Char
import Data.Word
import System.IO

import Challenge1
import Challenge3

key :: BS.ByteString
key = "YELLOW SUBMARINE"

main = do
    h <- openFile "7.txt" ReadMode
    c <- (toWordBS . htos . b64toh . B.filter (/= '\n')) `fmap` B.hGetContents h
    let k = initKey key
    print $ ecbDecrypt k c
    
-- Convert Char to Word8 based ByteString
toWordBS :: B.ByteString -> BS.ByteString
toWordBS = BS.pack . Prelude.foldr (\x a -> toEnum (ord x) : a) [] . B.unpack
    
initKey :: A.ByteArray key => key -> AES128
initKey k = case cipherInit k of
    CryptoPassed a -> a
    CryptoFailed e -> error (show e)