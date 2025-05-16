drop' :: Int -> a -> [a] -> [a]
drop' _ _ [] = []
drop' n _ xs | n >= 0 = drop n xs
drop' n c xs = [ c | _ <- [1 .. abs n] ] ++ xs

take' :: Int -> a -> [a] -> [a]
take' _ _ [] = []
take' n _ xs | n <= length xs = take n xs
take' n c xs = xs ++ [ c | _ <- [1 .. m] ]
  where
    m = abs $ length xs - n

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

halves :: [a] -> [[a]]
halves xs = [take (m + 1) xs] ++ [drop m xs]
  where
    m = length xs `div` 2

elemN :: Eq a => Int -> a -> [a] -> Bool
elemN n _ _ | n < 0 = False
elemN 1 e xs = e `elem` xs
elemN n e xs = if contains xs >= n then True else False
  where
    contains [] = 0
    contains (y:ys)
      | y == e = 1 + contains ys
      | otherwise = contains ys

mkLine :: [String] -> [[String]]
mkLine xs = map (\l -> l ++ map reverse l) ls
  where
    middleV = map (\l -> l !! (length l `div` 2) ) xs
    middleH = xs !! (length xs `div` 2)
    diagL = [ (xs !! i) !! i | i <- [0 .. length xs - 1] ]
    diagR = [ (xs !! i) !! j | (i, j) <- zip [0 .. length xs - 1] $ reverse [0 .. length xs - 1] ]

    ls = [ [ middleV, middleH ], [ diagL, diagR ] ]

xmas :: Int -> (Char -> Bool) -> [String] -> [[[String]]]
xmas n f xs = map mkLine $ getSegs xs 0
  where
    nRows = length xs
    nCols = length $ head xs
    
    getSegs [] _ = []
    getSegs (l:ls) row = seg ++ getSegs ls (succ row)
      where
        cols = [ i | (i, _) <- filter (\(_, c) -> f c) . zip [0 ..] $ l ]
        seg = mkSeg row cols
                           
    mkSeg _ [] = []
    mkSeg row (col:cols) = [map seg rows] ++ mkSeg row cols
      where
        a = n `div` 2
        b = n - a
        rows = drop' (row - a) "." . take' (row + b) "." $ xs
        seg = drop' (col - a) '.' . take' (col + b) '.'

part1, part2 :: String -> Int
part1 = length . filter ((==) "XMAS") . flatten . map halves . flatten . flatten . xmas 7 ((==) 'X') . lines
part2 = length . filter ((elemN 2) "MAS") . map last . xmas 3 ((==) 'A') . lines

main = readFile "input04" >>= print . part1
