module Day2.Main where

readInput = map (map (read @Int) . words) . lines

solve1 = length . filter isSafe
  where
    isSafe xs =
        (all (< 0) ys || all (> 0) ys)
        && all (\y -> 1 <= y && y <= 3) ys'
      where
        ys = diffs xs
        ys' = map abs ys

diffs xs = zipWith (-) xs (tail xs)

solve2 = undefined

main :: IO ()
main = do
    input <- readInput <$> readFile "Day2/test.txt"
    -- input <- readInput <$> readFile "Day2/input.txt"
    -- print input
    print $ solve1 input
    -- print $ solve2 input
