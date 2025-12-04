import Data.Array
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

maxMaybe :: Maybe Int -> Maybe Int -> Maybe Int
maxMaybe Nothing Nothing = Nothing
maxMaybe (Just a) Nothing = Just a
maxMaybe Nothing (Just b) = Just b
maxMaybe (Just a) (Just b) = Just (max a b)

-- bottom-up dynamic programming because top-down was too slow :c
buildMaxSubstring :: Int -> String -> Maybe Int
buildMaxSubstring _ "" = Nothing
buildMaxSubstring k s
  | k <= 0 = Just 0
  | k > n = Nothing
  | otherwise = dp ! (0, k)
  where
    digits = map digitToInt s
    n = length digits
    dp = array ((0, 0), (n, k)) [((pos, rem), entry pos rem) | pos <- [0 .. n], rem <- [0 .. k]]

    entry :: Int -> Int -> Maybe Int
    entry _ 0 = Just 0
    entry pos rem
      | pos == n = Nothing
      | rem > n - pos = Nothing
      | otherwise = maxMaybe takeVal skipVal
      where
        skipVal = dp ! (pos + 1, rem)
        takeVal =
          case dp ! (pos + 1, rem - 1) of
            Nothing -> Nothing
            Just v -> Just (digits !! pos * 10 ^ (rem - 1) + v)

partOne = sum . map (fromMaybe 0 . buildMaxSubstring 2)

partTwo = sum . map (fromMaybe 0 . buildMaxSubstring 12)

main :: IO ()
main = do
  contents <- readFile "./day3/input.txt"
  let input = lines contents
  print $ partOne input
  print $ partTwo input