smallest :: [Int] -> Int
smallest [] = 0
smallest lst = foldl1 min lst

largest :: [Int] -> Int
largest [] = 0
largest lst = foldl1 max lst

computeDiff :: [Int] -> Int
computeDiff lst = largest lst - smallest lst

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
    print . sum . map (computeDiff . parseLine) $ input
