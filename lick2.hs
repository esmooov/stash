{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T
import qualified Text.Regex as R
import Text.Regex.Posix
import Data.List
import qualified Data.Char as C

isClassChar a = C.isAlphaNum a || C.isSpace a || a == '\'' || a == '-'

cullWord :: BL.ByteString -> B.ByteString
cullWord w = BL.foldl (\stack char -> if isClassChar char then B.snoc stack (C.toLower char) else stack) "" w

incrementOrAdd :: B.ByteString -> Int -> Maybe Int -> Maybe Int
incrementOrAdd s v lup = case lup of 
                            Just old -> Just (v + old)
                            Nothing  -> Just 1

updateWord :: BL.ByteString -> T.Trie Int -> T.Trie Int
updateWord w t = T.alterBy incrementOrAdd word 1 t
                   where word = cullWord w

procTextN :: Int -> BL.ByteString -> [(B.ByteString,Int)]
procTextN n t = T.toList $ snd $ foldl ngram (base,T.empty) words
                where words = BL.split ' ' $ BL.map (\a -> if a == '\n' then ' ' else a) t
                      base = replicate (n-1) ""
                      ngram (st@(s:ss),tr) word = (ss++[word],updateWord (whiteMerge st word) tr)

whiteMerge :: [BL.ByteString] -> BL.ByteString -> BL.ByteString
whiteMerge stack word = {-# SCC "mean" #-} BL.intercalate " " (stack ++ [word])

procText :: BL.ByteString -> [(B.ByteString,Int)]
procText t = T.toList $ foldr updateWord T.empty words
             where words = BL.split ' ' $ BL.map (\a -> if a == '\n' then ' ' else a) t

main = do
       test2 <- BL.readFile "jobs"
       print $ sortBy (\(a,b) (c,d) -> compare d b) $ procText test2
