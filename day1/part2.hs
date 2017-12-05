applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldl (.) id (replicate n f)

cycleOnce :: String -> String
cycleOnce "" = ""
cycleOnce (x:xs) = xs ++ [x]

captcha :: String -> Int
captcha "" = 0
captcha lst = sum . map (read . (:[]) . fst) . filter (\(a, b) -> a == b) $ zip lst newLst
    where
    newLst = applyNTimes ((length lst) `div` 2) cycleOnce lst

main = do
    input <- getLine
    print $ captcha input
