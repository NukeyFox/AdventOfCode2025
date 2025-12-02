nub list = nubHelper list []
  where
    nubHelper [] acc = acc
    nubHelper (x : xs) acc
      | x `elem` acc = nubHelper xs acc
      | otherwise = nubHelper xs (x : acc)

splitOn sep = foldr step [[]]
  where
    step c acc
      | c == sep = [] : acc
      | otherwise = (c : head acc) : tail acc

parseInput :: String -> [(String, String)]
parseInput input = map ((\(x : y : _) -> (x, y)) . splitOn '-') (splitOn ',' input)

splitKWays str k = splitKWaysHelper str k "" []
  where
    splitKWaysHelper "" _ acc ret = acc : ret
    splitKWaysHelper (x : xs) k acc ret
      | length acc == splitLength = splitKWaysHelper (x : xs) k "" (acc : ret)
      | otherwise = splitKWaysHelper xs k (x : acc) ret
    splitLength = length str `div` k

nthSplit string n
  | len < n = Nothing
  | len `mod` n /= 0 = Nothing
  | otherwise = Just (splitKWays string n)
  where
    len = length string

stringValid k string = maybe False (\(x : xs) -> foldl (\acc y -> acc && y == x) True xs) $ nthSplit string k

validityFilter :: Int -> (String, String) -> [Int]
validityFilter k (str1, str2) = nub $ map read $ filter helper (map show [(read str1 :: Int) .. (read str2 :: Int)])
  where
    helper = stringValid k

partOne :: [(String, String)] -> Int
partOne = sum . concatMap (validityFilter 2)

partTwo = sum . concatMap (\pair -> nub $ concatMap ((\f -> f pair) . validityFilter) [2 .. 12])

main :: IO ()
main = do
  contents <- readFile "./day2/input.txt"
  let input = parseInput contents
  print $ partOne input
  print $ partTwo input
