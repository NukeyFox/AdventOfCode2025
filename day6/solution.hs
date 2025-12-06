import Data.Bifunctor (Bifunctor (bimap, second))
import Data.List (transpose)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn c arr = splitOnHelper c arr [] []
  where
    splitOnHelper c [] acc ret = if null acc then ret else reverse acc : ret
    splitOnHelper c (x : xs) acc ret
      | c == x && null acc = splitOnHelper c xs [] ret
      | c == x = splitOnHelper c xs [] (reverse acc : ret)
      | otherwise = splitOnHelper c xs (x : acc) ret

funcParse :: String -> (Int -> Int -> Int)
funcParse "+" = (+)
funcParse "*" = (*)
funcParse _ = error "Unsupported operator"

parseInputPartOne :: String -> ([String], [[String]])
parseInputPartOne =
  second transpose
    . (\x -> (last x, init x))
    . map (splitOn ' ')
    . lines

parseInputPartTwo :: String -> ([String], [[String]])
parseInputPartTwo =
  bimap (splitOn ' ') (splitOn "" . map (filter (/= ' ')) . transpose)
    . (\x -> (last x, init x))
    . lines

solve :: ([String], [[String]]) -> Int
solve =
  sum
    . map (uncurry foldl1)
    . uncurry zip
    . bimap (map funcParse) (map (map read))

main :: IO ()
main = do
  contents <- readFile "./day6/input.txt"
  let partOne = parseInputPartOne contents
  let partTwo = parseInputPartTwo contents
  print $ solve partOne
  print $ solve partTwo