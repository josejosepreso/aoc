import Text.Regex.Posix
import Data.List

type Input = ([(Int, Int)], [[Int]])

solve :: (Bool -> Bool) -> ([Int] -> [Int]) -> Input -> Int
solve f g (rules, pages) = sum . map middleE . map g . filter (f . ok) $ pages
  where
    ok :: [Int] -> Bool
    ok (x:xs) = checkRules ([x], xs)

    checkRules (_, []) = True
    checkRules (prev, curr:rest) = checkRules (prev ++ [curr], rest)
                                   && and [ not $ elem b prev | (_, b) <- filter (\(a, b) -> a == curr) rules ]

    middleE xs = xs !! (length xs `div` 2)
    
handleInput :: String -> Input
handleInput input = (rules, pages)
  where 
    xs = lines input
    rules = map mkPair . takeWhile (/= "") $ xs
    pages = map (read :: String -> [Int]) . map (\s -> '[' : s ++ "]") . tail . dropWhile (/= "") $ xs

    mkPair s = let (_, first, second) = s =~ "[0-9]+|[0-9]+" :: (String, String, String)
               in (read first :: Int, read (tail second) :: Int)

part1 = solve id  id . handleInput

part2 s = let (rules, pages) = handleInput s
          --in solve not order (rules, pages)
          in rules
  where order xs = xs

main = readFile "input05" >>= print . part1
