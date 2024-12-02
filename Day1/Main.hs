module Day1.Main where

import Data.List (transpose, sort, group)
import Data.Function ((&), on)
import Data.IntMap (fromAscList, toList, intersectionWith, IntMap)

type Input = ([Int], [Int])
type Output = Int

readInput :: String -> Input
readInput = toPair . transpose . map parseRow . lines 
  where
    parseRow = map (read @Int) . words
    toPair [ls, rs] = (ls, rs)

solve1 :: Input -> Output
solve1 = sum . distances
  where
    distances = map abs . uncurry differences
    differences = zipWith (-) `on` sort

solve2 :: Input -> Output
solve2 = sum . map toScore . totalFreqById
  where
    toScore = uncurry (*)
    totalFreqById = toList . uncurry combineFreqs
    combineFreqs = intersectionWith (*) `on` freqs

freqs :: [Int] -> IntMap Int
freqs = fromAscList . map toCounts . group . sort
  where
    toCounts xs = (head xs, length xs)


main :: IO ()
main = do
    inputRaw <- readFile "Day1/input.txt"
    let input = readInput inputRaw
        res1 = solve1 input
        res2 = solve2 input
    print res1
    print res2
