{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import Text.Regex.Posix
import Data.List
import qualified Data.Char as C


isClassChar a = C.isAlphaNum a || elem a " \'-#@%"

cullWord :: B.ByteString -> B.ByteString
cullWord w = B.map C.toLower $ B.filter isClassChar w

procTextN :: Int -> B.ByteString -> [([B.ByteString],Int)]
procTextN n t = H.toList $ foldl' ngram H.empty lines
                 where !lines = B.lines $ cullWord t
                       ngram tr line = snd $! foldl' breakdown (base,tr) (B.split ' ' line)
                       base = replicate (n-1) ""

breakdown :: ([B.ByteString], H.HashMap [B.ByteString] Int) -> B.ByteString -> ([B.ByteString], H.HashMap [B.ByteString] Int)
breakdown (!st@(s:ss),tree) !word = (newStack,expandedWord)
                                    where !newStack = ss++[word]
                                          !expandedWord = updateWord (st++[word]) tree

updateWord :: [B.ByteString] -> H.HashMap [B.ByteString] Int -> H.HashMap [B.ByteString] Int
updateWord !w = H.insertWith (+) w 1

procText :: B.ByteString -> [([B.ByteString],Int)]
procText t = H.toList $ foldl' (\hash word -> updateWord [word] hash) H.empty words
             where words = B.split ' ' $ B.map (\a -> if a == '\n' then ' ' else a) t

main = do
       test2 <- B.readFile "canewobble"
       print $ filter (\(a,b) -> b > 100) $ sortBy (\(a,b) (c,d) -> compare d b) $ procTextN 3 test2
