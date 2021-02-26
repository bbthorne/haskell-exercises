{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where
import Buffer
import JoinList
import Sized
import Scrabble

constructEntry :: String -> JoinList (Score,Size) String
constructEntry line = Single (scoreString line, Size 1) line

instance Buffer (JoinList (Score,Size) String) where
  toString = foldr addLine "" . jlToList
    where addLine line doc = line ++ "\n" ++ doc

  fromString = foldr (+++) Empty . map constructEntry . lines

  line = indexJ

  replaceLine i nl jl = takeJ i jl +++ constructEntry nl +++ dropJ (i + 1) jl

  numLines = getSize . size . snd . tag

  value = scoreV . fst . tag

initialize :: [String] -> JoinList (Score,Size) String
initialize = fromString . unlines
