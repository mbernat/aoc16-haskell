module Day15 where

input :: [(Integer, Integer)]
input = [(7, 0), (13, 0), (3, 2), (5, 2), (17, 0), (19, 7), (11, 0)]

rotate :: Integer -> (Integer, Integer) -> (Integer, Integer)
rotate r (n, x) = (n, (x + r) `mod` n)

rotated :: [(Integer, Integer)]
rotated = map (uncurry rotate) $ zip [1..] input

obvs = 35 + 3 * 5 * 7

next = obvs + 10 * 3 * 5 * 7 * 19

solve1 = next + 3 * 3 * 5 * 7 * 17 * 19 -1

solve2 = solve1 +  7 * 3 * 5 * 7 * 13 * 17 * 19