import Data.Array (Ix (inRange))
import Data.Bifunctor (Bifunctor (bimap))
import Data.List (sortBy)

parseInput :: String -> ([(Int, Int)], [Int])
parseInput = bimap (map toRange) (map read . tail) . break ("" ==) . lines
  where
    toRange = bimap read (read . tail) . break ('-' ==)

mergeRanges [] = []
mergeRanges [x] = [x]
mergeRanges ((a, b) : (c, d) : xs)
  | b >= c && b >= d = mergeRanges ((a, b) : xs)
  | b >= c && b <= d = mergeRanges ((a, d) : xs)
  | b < c && b < d = (a, b) : mergeRanges ((c, d) : xs)

partOne = uncurry (\ranges -> length . filter (\x -> any (`inRange` x) ranges))

partTwo = sum . map ((1 +) . uncurry (flip (-))) . mergeRanges . sortBy (\(a, b) (c, d) -> compare a c) . fst

main :: IO ()
main = do
  contents <- readFile "./day5/input.txt"
  let input = parseInput contents
  print $ partOne input
  print $ partTwo input