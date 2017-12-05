captcha :: String -> Int
captcha "" = 0
captcha (x:xs) = sum . map (read . (:[]) . fst) . filter (\(a, b) -> a == b) $ zip (x:xs) (xs ++ [x])

main = do
    input <- getLine
    print $ captcha input
