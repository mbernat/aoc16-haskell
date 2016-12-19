module Day18 where

check :: (Bool, Bool) -> Bool
check (a, c) = a == not c

count = length . filter (== False)

step :: ([Bool], Int) -> ([Bool], Int)
step (a, n) = (b, n + count b)
  where
    b = map check $ zip (False:a) (tail a ++ [False])

fromString :: String -> ([Bool], Int)
fromString s = (bs, count bs)
  where
    bs = map (== '^') s

input0 = ".^^.^.^^^^"

input1 =
  ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^."

solve input row = snd (rows !! (row - 1))
  where
    rows = iterate step (fromString input)

play n = solve input1 n - 50 * n
