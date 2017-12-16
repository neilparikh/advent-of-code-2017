data Direction = R
               | U
               | L
               | D
               deriving Enum

turn :: Direction -> Direction
turn D = R
turn x = succ x

location :: Int -> (Int, Int)
location i = go 1 R 1 0 0 i (0, 0)
    where
    go curr dir 0             len 0    goal loc = go curr (turn dir) len       0 1 goal loc
    go curr dir 0             len 1    goal loc = go curr (turn dir) (len + 1) 0 0 goal loc
    go curr dir stepsTillTurn len incr goal loc | goal == curr = loc
    go curr R   stepsTillTurn len incr goal (x, y) = go (curr + 1) R (stepsTillTurn - 1) (len + 1) incr goal (x + 1, y)
    go curr U   stepsTillTurn len incr goal (x, y) = go (curr + 1) U (stepsTillTurn - 1) (len + 1) incr goal (x, y + 1)
    go curr L   stepsTillTurn len incr goal (x, y) = go (curr + 1) L (stepsTillTurn - 1) (len + 1) incr goal (x - 1, y)
    go curr D   stepsTillTurn len incr goal (x, y) = go (curr + 1) D (stepsTillTurn - 1) (len + 1) incr goal (x, y - 1)


main = do
    input <- read <$> getLine
    print . (\(x, y) -> abs x + abs y) . location $ input
