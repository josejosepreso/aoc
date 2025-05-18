findGuard :: [String] -> Int -> (Int, Int, Char)
findGuard [] _ = (-1, -1, '\0')
findGuard (y:ys) row
  | null stat = findGuard ys $ succ row
  | otherwise = (row, fst $ head stat, snd $ head stat)
  where stat = filter (\(_, c) -> c `elem` "v^><") . zip [0 ..] $ y

mkTable :: [String] -> Char -> (Int, Int) -> [String]
mkTable xs char (top, left) = take top xs
                              ++ [ take left row ++ [char] ++ (tail $ drop left row) ]
                              ++ (drop (top + 1) xs)
  where row = xs !! top

solve :: Char -> [String] -> Int
solve target xs = sum . map (length . filter (== target)) . positions xs iniChar $ (iniRow, iniCol)
  where
    (iniRow, iniCol, iniChar) = findGuard xs 0

    (maxRow, maxCol) = (length xs, length $ head xs)

    positions _ _ (-1, -1) = []
    positions tab char (row, col) | or [ char == '^' && row == 0
                                       , char == '<' && col == 0
                                       , char == 'v' && row == maxRow - 1
                                       , char == '>' && col == maxCol - 1
                                       ] = tab
    positions tab char (row, col) = case char of
      '^' -> step (row - 1, col) '>'
      '<' -> step (row, col - 1) '^'
      '>' -> step (row, col + 1) 'v'
      _ -> step (row + 1, col) '<'
      where    
        step next ch
          | prev next && target == 'O' = positions (mkTable tab 'O' next) char next
          | notObstruction next = positions (mkTable tab 'X' next) char next
          | otherwise = positions tab ch (row, col)
        
        notObstruction (top, left) = (tab !! top) !! left /= '#'

        prev (top, left) = ((tab !! top) !! left) `elem` "XO"

part1 = solve 'X' <$> lines <$> readFile "input06"
part2 = undefined -- solve 'O' <$> lines <$> readFile "input06_"
