captcha :: String -> Int
captcha "" = 0
captcha lst = sum . map (read . (:[]) . fst) . filter (\(a, b) -> a == b) $ zip lst newLst
    where
    len = length lst
    newLst = take len . drop (len `div` 2) . cycle $ lst

main = do
    input <- getLine
    print $ captcha input
