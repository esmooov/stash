{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T
import qualified Text.Regex as R
import Text.Regex.Posix
import Data.List
import qualified Data.Char as C

isClassChar a = C.isAlphaNum a || C.isSpace a || a == '\'' || a == '-'

cullWord :: B.ByteString -> B.ByteString
cullWord w = B.map C.toLower $ B.filter isClassChar w

incrementOrAdd :: B.ByteString -> Int -> Maybe Int -> Maybe Int
incrementOrAdd s v lup = case lup of 
                            Just old -> Just (v + old)
                            Nothing  -> Just 1

updateWord :: B.ByteString -> T.Trie Int -> T.Trie Int
updateWord w t = T.alterBy incrementOrAdd word 1 t
                   where word = cullWord w

procTextN :: Int -> B.ByteString -> [(B.ByteString,Int)]
procTextN n t = T.toList $ snd $ foldl ngram (base,T.empty) words
                where words = B.split ' ' $ B.map (\a -> if a == '\n' then ' ' else a) t
                      base = replicate (n-1) ""
                      ngram (st@(s:ss),tr) word = (ss++[word],updateWord (whiteMerge st word) tr)

whiteMerge :: [B.ByteString] -> B.ByteString -> B.ByteString
whiteMerge stack word = B.intercalate " " (stack ++ [word])

procText :: B.ByteString -> [(B.ByteString,Int)]
procText t = T.toList $ foldl' (\trie word -> updateWord word trie) T.empty words
             where words = B.split ' ' $ B.map (\a -> if a == '\n' then ' ' else a) t

main = do
       test2 <- B.readFile "jobs"
       print $ sortBy (\(a,b) (c,d) -> compare d b) $ procText test2
