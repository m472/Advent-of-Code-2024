pairwise :: [Int] -> [(Int, Int)]
pairwise [_] = []
pairwise (x1 : x2 : xs) = (x1, x2) : pairwise (x2 : xs)

isSafe :: [Int] -> Bool
isSafe xs =
    let
        diff = map (uncurry (-)) $ pairwise xs
        isMonotonous = all ((\n -> n >= 1 && n <= 3) . abs) diff
        allPositiveOrAllNegative = (all (> 0) diff || all (< 0) diff)
     in
        isMonotonous && allPositiveOrAllNegative

isSafeDamped :: [Int] -> Bool
isSafeDamped xs = isSafeDamped' 1 (if head xs < last xs then xs else reverse xs)
  where
    isSafeDamped' :: Int -> [Int] -> Bool
    isSafeDamped' _ [] = True
    isSafeDamped' _ [a] = True
    isSafeDamped' n (a : b : rest) =
        let
            safe = (b - a) >= 1 && (b - a) <= 3
         in
            if safe
                then isSafeDamped' n (b : rest)
                else
                    let
                        n' = n - 1
                     in
                        (n' >= 0) && (isSafeDamped' n' (a : rest))

main :: IO ()
main = do
    input <- map (map read . words) . lines <$> readFile "../example.txt"
    print $ length $ filter isSafe input
    print $ length $ filter isSafeDamped input
