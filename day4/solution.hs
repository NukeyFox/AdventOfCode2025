import Data.Array

parseInput :: String -> Array (Int, Int) Char
parseInput input = listArray ((1, 1), (n, m)) (concat l)
  where
    n = length (head l)
    m = length l
    l = lines input

cellsAround (x, y) arr =
  [ (i, j)
    | i <- [x - 1 .. x + 1],
      w >= i,
      i >= a,
      j <- [y - 1 .. y + 1],
      h >= j,
      j >= b,
      i /= x || j /= y
  ]
  where
    (a, b) = fst $ bounds arr
    (w, h) = snd $ bounds arr

paperCountAroundCells (x, y) arr = length $ filter (== '@') $ map (arr !) (cellsAround (x, y) arr)

getRemovablePaper arr = filter ((< 4) . (`paperCountAroundCells` arr)) (filter ((== '@') . (arr !)) (range $ bounds arr))

removePaper :: (Int, Array (Int, Int) Char) -> (Int, Array (Int, Int) Char)
removePaper (count, state) = (count + length rem, foldl (//) state (map ((: []) . (,'.')) rem))
  where
    rem = getRemovablePaper state

partOne = length . getRemovablePaper

partTwo = fst . until (null . getRemovablePaper . snd) removePaper . (0,)

main :: IO ()
main = do
  contents <- readFile "./day4/input.txt"
  let input = parseInput contents
  print $ partOne input
  print $ partTwo input