module Crypto
(
   getFreq
  ,fixedXOR
  ,hexToBytes
  ,bytesToHex
  ,decryptSingleCharXOR
  ,singleCharXOR
  ,stringToBytes
  ,repKeyXOR
  ,base64ToBytes
  ,decryptRepKeyXOR
) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Codec.Binary.Base64 as Six
import Data.Word(Word8)
import Data.Bits(xor,popCount)
import Data.Maybe
import Numeric(readHex,showHex)
import Control.Exception(assert)
import Debug.Trace

type FreqMap = M.Map Word8 Float

decryptSingleCharXOR :: FreqMap -> [Word8] -> (Char,String,Float)
decryptSingleCharXOR freqs s = L.minimumBy (\(x,y,z) (a,b,c) -> compare z c) scores
  where scores = map (\(x,y)-> (x, y, compareFreq (getFreq y) freqs)) $ poss
        poss = map (\x -> (B.w2c x,bytesToString $ singleCharXOR x s)) [0..255]

decryptRepKeyXOR :: FreqMap -> [Word8] -> (String,String)
decryptRepKeyXOR freqs s = (key,message)
  where message = bytesToString $ repKeyXOR (stringToBytes key) s
        key =  map ((\(x,y,z) -> (x)) . decryptSingleCharXOR freqs) $ L.transpose  $ splitListEq bestSize s
        bestSize = fst $ L.minimumBy (\(x,y) (a,b) -> compare y b) scoredSizes
        scoredSizes = map (\x -> (x,checkSize x)) [2..40]
        checkSize n =  average $ map (\(a,b) -> average $ zipWith hamDist a b) $ map (splitAt n) $ splitListEq (2*n) s 

splitListEq :: Int -> [a] -> [[a]]
splitListEq n x 
  | length x >= n =  fst split : splitListEq n (snd split)
  | otherwise    = []
  where split = splitAt n x 

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / L.genericLength xs

gHamDist :: [Word8] -> [Word8] -> Int
gHamDist a b   
  | length a == length b = sum $ zipWith hamDist a b
  | otherwise            = assert False $ 0

hamDist :: Word8 -> Word8 -> Int
hamDist a b = popCount $ a `xor` b 

hexToBytes :: String -> [Word8]
hexToBytes s = map (fst.head.readHex) (breakStr s)

bytesToBase64 :: [Word8] -> String
bytesToBase64 = Six.encode

base64ToBytes :: String -> Maybe [Word8]
base64ToBytes = Six.decode

bytesToHex :: [Word8] -> String
bytesToHex = concatMap (\x ->showHex x "")

bytesToString :: [Word8] -> String
bytesToString = map B.w2c

stringToBytes :: String -> [Word8]
stringToBytes = map B.c2w

breakStr :: String -> [String]
breakStr str 
  | length str `mod` 2 == 1 = [head str] : breakStr' (tail str)
  | otherwise = breakStr' str
  where breakStr' (x:y:xs) = [x,y] : breakStr' xs
        breakStr' _ = []

fixedXOR :: [Word8] -> [Word8] -> [Word8]
fixedXOR a b 
  | length a == length b = fixedXOR' a b
  | otherwise = assert False $ []
  where fixedXOR' (a:as) (b:bs) = a `xor` b : fixedXOR' as bs
        fixedXOR' _ _ = []

singleCharXOR :: Word8 -> [Word8] -> [Word8]
singleCharXOR c [] = []
singleCharXOR c (a:as) = c `xor` a : singleCharXOR c as

repKeyXOR :: [Word8] -> [Word8] -> [Word8]
repKeyXOR key str = repKeyXOR' key str
  where repKeyXOR' (k:ks) (s:ss) = k `xor` s : repKeyXOR' ks ss
        repKeyXOR' [] s = repKeyXOR' key s
        repKeyXOR' _ [] = []

compareFreq :: FreqMap -> FreqMap -> Float
compareFreq a b = sum $ zipWith (\(a,b) (c,d) -> abs $ b-d) (M.toAscList a) (M.toAscList b)

getFreq :: String -> FreqMap
getFreq s = normalize $ L.foldl' addChar initalMap sw
  where addChar freqs c 
          | c `M.member` freqs = M.adjust (1+) c freqs 
          | otherwise = M.insert c 1 freqs
        normalize freqs = M.map (\x-> fromIntegral x/ fromIntegral total) freqs
          where total = sum . M.elems $ freqs 
        initalMap = M.fromList [ (x,0) | x <- [0..255] ]
        sw = stringToBytes s 
