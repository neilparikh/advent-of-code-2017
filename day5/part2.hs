-- FIXME: This is pretty slow

import qualified NonEmptyZipper as NEZ

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)

step :: (NEZ.Zipper Int, Int) -> Maybe (NEZ.Zipper Int, Int)
step (NEZ.Zipper l 0 r, i) = Just (NEZ.Zipper l 1 r, i + 1)
step (z@(NEZ.Zipper l x r), i)
    | x < 0 = if length l < (abs x)
              then Nothing
              else Just . (\x -> (x, i + 1)) . applyNTimes (abs x) NEZ.goLeft . NEZ.updateCurrent (+1) $ z
    | x >= 3 = if length r < x
              then Nothing
              else Just . (\x -> (x, i + 1)) . applyNTimes x NEZ.goRight . NEZ.updateCurrent (subtract 1) $ z
    | x > 0 = if length r < x
              then Nothing
              else Just . (\x -> (x, i + 1)) . applyNTimes x NEZ.goRight . NEZ.updateCurrent (+1) $ z

maybeFold :: (a -> Maybe a) -> a -> a
maybeFold f a = case f a of
    Just a' -> maybeFold f a'
    Nothing -> a

main = do
    input <- NEZ.fromList . map read . lines <$> getContents
    print . snd $ maybeFold step (input, 1)
