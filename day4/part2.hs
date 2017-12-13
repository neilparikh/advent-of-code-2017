import Data.List (nub, sort)

check :: [String] -> Bool
check orig = length orig == (length . nub) orig

main = do
    raw_input <- getContents
    let input = map (map sort . words) $ lines raw_input
    print . length $ filter check input
