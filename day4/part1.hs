import Data.List (nub)

check :: [String] -> Bool
check orig = length orig == (length . nub) orig

main = do
    raw_input <- getContents
    let input = map (words) $ lines raw_input
    print . length $ filter check input
