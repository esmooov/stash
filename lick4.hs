{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import Text.Regex.Posix
import Data.List
import qualified Data.Char as C
import Control.Applicative
import Data.Binary as Bin

isClassChar a = C.isAlphaNum a || elem a " \'-#@%"

cullWord :: B.ByteString -> B.ByteString
cullWord w = B.map C.toLower $ B.filter isClassChar w

procTextN :: Int -> B.ByteString -> [(B.ByteString,Int)]
procTextN n t = H.toList $ foldl' ngram' H.empty lines
                 where !lines = B.lines t
                       ngram' !tr !line = ngram tr (B.words line) n

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
       test2 <- cullWord <$> B.readFile "canewobble"
       print $ filter (\(a,b) -> b > 100) $ sortBy (\(a,b) (c,d) -> compare d b) $ procTextN 3 test2
