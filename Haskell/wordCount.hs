module Main (
   main
) where

import Data.Word
import System.IO
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as DM

-- The main function builds the map from words to counts, then
-- outputs the result of converting the map to a list, sorting
-- it by values and showing the first 10
main :: IO()
main = do
   map <- buildMapFromInput (DM.fromList [])
   putStrLn(display $ take 10 $ sortListMap $ DM.toList(map))

display :: [(S.ByteString,Int)] -> String
display m = concat (map (\(w,i) -> C.unpack w ++ ": " ++ (show i) ++ "\n") m)

-- Builds the word map from stdin
buildMapFromInput :: DM.Map S.ByteString Int -> IO(DM.Map S.ByteString Int)
buildMapFromInput m = do
   con <- S.getContents
   return $ addWordsToMap (findWords $ S.map toLower con) m

toLower :: Word8 -> Word8
toLower i = if i > 0x40 && i < 0x5B then i + 0x20 else i

-- Finds the words in a given ByteString according to the rule that
-- anything not a letter is a separator
findWords :: S.ByteString -> [S.ByteString]
findWords bs = case (S.null bs) of
   True -> []
   False -> (S.takeWhile isAlpha bs') : findWords (S.dropWhile isAlpha bs')
      where
         bs' = S.dropWhile (not.isAlpha) bs

isAlpha :: Word8 -> Bool
isAlpha c = c > 0x60 && c < 0x7B

-- Adds a list of words to the current map, incrementing the counter
-- of any words already present
addWordsToMap :: [S.ByteString] -> DM.Map S.ByteString Int -> DM.Map S.ByteString Int
addWordsToMap [] m = m
addWordsToMap (word:ws) m = addWordsToMap ws m'
   where
      m' = DM.insertWith (+) word 1 m

-- Quicksort on values in descending order
sortListMap :: [(S.ByteString,Int)] -> [(S.ByteString,Int)]
sortListMap [] = []
sortListMap ((s,i):sis) = sortListMap [(gs,gi) | (gs,gi) <- sis, gi > i] 
                           ++ [(s,i)] ++
                          sortListMap [(ls,li) | (ls,li) <- sis, li <= i]

