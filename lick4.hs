{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as H
import Text.Regex.Posix
import Data.List
import qualified Data.Char as C
import Control.Applicative
import Data.Binary
import Data.Binary.Get
import Data.Int

getChunkList :: Int -> Get [B.ByteString]
getChunkList n = do
    left <- remaining
    if let reach = fromIntegral n in left < reach
        then do x <- getBytes $ fromIntegral left
                return [x]
        else do x <- getBytes n
                rest <- getChunkList n
                return (x:rest)

isClassChar a = C.isAlphaNum a || elem a " \'-#@%"

cullWord :: BL.ByteString -> BL.ByteString
cullWord w = BL.map C.toLower $ BL.filter isClassChar w

procTextN :: H.HashMap B.ByteString Int -> Int -> B.ByteString ->  H.HashMap B.ByteString Int
procTextN tree n t = foldl' ngram' tree lines
                     where !lines = B.lines t
                           ngram' !tr !line = ngram tr (B.words line) n

procTextN' :: Int -> [B.ByteString] -> [(B.ByteString,Int)]
procTextN' n chunks = H.toList $ foldl' (\tr c -> H.filter ( > 2) $ procTextN tr n c) H.empty chunks

ngram :: H.HashMap B.ByteString Int -> [B.ByteString] -> Int -> H.HashMap B.ByteString Int
ngram tr []           _  = tr    
ngram tr words@(w:ws) n  = ngram newtree ws n
                           where woz = take n words
                                 !newtree = updateWord woz tr

updateWord :: [B.ByteString] -> H.HashMap B.ByteString Int -> H.HashMap B.ByteString Int
updateWord !w = H.insertWith (+) (B.intercalate " " w) 1

procText :: B.ByteString -> [(B.ByteString,Int)]
procText t = H.toList $ foldl' (\hash word -> updateWord [word] hash) H.empty words
             where words = B.split ' ' $ B.map (\a -> if a == '\n' then ' ' else a) t

main = do
       test2 <- runGet (getChunkList 1000000) <$> cullWord <$> BL.readFile "canewobble"
       print $ filter (\(a,b) -> b > 100) $ sortBy (\(a,b) (c,d) -> compare d b) $ procTextN' 3 test2
