intercalateL :: [a] -> [a] -> [a]
intercalateL xs [] = xs
intercalateL [] ys = ys
intercalateL (x:xs) (y:ys) = x:y:(intercalateL xs ys)

operator :: [String] -> [String] -> [[String]]
operator symbols xs = [ intercalateL xs ys | ys <- string $ length xs - 1 ]
  where
    string n = mkString 0 []
      where
        mkString m s | m == n = [s]
        mkString m s = flatten [ mkString (m + 1) (symbol : s) | symbol <- symbols ]

    flatten [] = []
    flatten (x:xs) = x ++ flatten xs

eval :: [String] -> Int
eval = result
  where
    result :: [String] -> Int
    result [a] = read a :: Int
    result (x:symbol:y:xs) = result $ op ++ xs
      where
        n = read x :: Int
        m = read y :: Int
        op = case symbol of
               "+" -> [show $ n + m]
               "*" -> [show $ n * m]
               "||" -> [x ++ y]

true :: (Int, [[String]]) -> Bool
true (result, arr) = result `elem` xs
  where xs = map eval arr

handleInput :: String -> (Int, [String])
handleInput s = ( read (takeWhile (/= ':') s) :: Int
                , words . tail . dropWhile (/= ':') $ s
                )

solve ops =
  sum
  <$> map fst
  <$> filter true
  <$> map (\(r, arr) -> (r, operator ops arr))
  <$> map handleInput
  <$> lines
  <$> readFile "input07"

-- time ~ 13.22s
part1 = solve ["+", "*"]
-- time ~ 9.4m
part2 = solve ["+", "*", "||"]

main = part2 >>= print
