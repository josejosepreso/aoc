import Data.List

lists :: [[String]] -> ([Int], [Int])
lists xs = line xs [] []
  where
    line [] a b = (sort a, sort b)
    line ([y, w]:ys) a b = line ys (n:a) (m:b)
      where n = read y :: Int
            m = read w :: Int
      
distances :: [(Int, Int)] -> Int
distances xs = sum [ abs $ a - b | (a, b) <- xs ]

similarity :: ([Int], [Int]) -> Int
similarity (x:xs, ys) = sum [ x * (length . filter (== x) $ ys) | x <- xs ]

part1 = distances . (\(a, b) -> zip a b) . lists
part2 = similarity . lists

main = map words <$> lines <$> readFile "input01" >>= print . part2
