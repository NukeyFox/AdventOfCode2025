commandToInt :: String -> Int
commandToInt ('L' : xs) = -(read xs :: Int)
commandToInt ('R' : xs) = read xs :: Int
commandToInt _ = 0

parseInput = map commandToInt . lines

partOneCond state x = if (state + x) `mod` 100 == 0 then 1 else 0

partTwoCond state x = if abs x >= k then 1 + ((abs x - k) `div` 100) else 0
  where
    k
      | x > 0 = 100 - state
      | state == 0 = 100
      | otherwise = state

zeroCounter :: (Int -> Int -> Int) -> (Int, Int) -> Int -> (Int, Int)
zeroCounter zeroCond (acc, state) x = (acc + zeroCond state x, (state + x) `mod` 100)

partOne input = fst $ foldl (zeroCounter partOneCond) (0, 50) input

partTwo input = fst $ foldl (zeroCounter partTwoCond) (0, 50) input

main :: IO ()
main = do
  contents <- readFile "./day1/input.txt"
  let input = parseInput contents
  print $ partOne input
  print $ partTwo input
