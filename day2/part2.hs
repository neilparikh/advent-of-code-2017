import Data.List (sort)

findPairResult :: [Int] -> Int
findPairResult [] = 0
findPairResult (x:xs) = case (checkForMatch xs) of
    [] -> findPairResult xs
    [b] -> x `div` b
    _ -> error "should not happen"
    where
    checkForMatch :: [Int] -> [Int]
    checkForMatch = filter (\b -> x `mod` b == 0)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy sep (x:xs)
    | x == sep = splitBy sep xs
    | otherwise = (x:(takeWhile (/= sep) xs)):(splitBy sep (dropWhile (/= sep) xs))

parseLine :: String -> [Int]
parseLine = map read . splitBy '\t'

main :: IO ()
main = do
    raw_input <- getContents
    let input = lines raw_input
    print . sum . map (findPairResult . reverse . sort . parseLine) $ input
