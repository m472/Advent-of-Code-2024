import Data.List (elemIndex, partition, sortBy)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn = splitOn' []
  where
    splitOn' :: (Eq a) => [a] -> a -> [a] -> [[a]]
    splitOn' acc _ [] = [reverse acc]
    splitOn' acc x (y : ys)
        | x == y = reverse acc : splitOn' [] x ys
        | otherwise = splitOn' (y : acc) x ys

checkRule :: [Int] -> [Int] -> Bool
checkRule rule update = case map (`elemIndex` update) rule of
    [Just idxA, Just idxB] -> idxA < idxB
    _ -> True

getMiddle :: [a] -> a
getMiddle xs
    | length xs `mod` 2 == 1 = xs !! (length xs `div` 2)
    | otherwise = undefined

swapElems :: Int -> Int -> [a] -> [a]
swapElems a b xs
    | a == b = xs
    | a < b = take a xs ++ [xs !! b] ++ take (b - (a + 1)) (drop (a + 1) xs) ++ [xs !! a] ++ drop (b + 1) xs
    | otherwise = swapElems b a xs

pageOrdering :: [[Int]] -> (Int -> Int -> Ordering)
pageOrdering rules a b
    | [a, b] `elem` rules = LT
    | [b, a] `elem` rules = GT
    | otherwise = EQ

main :: IO ()
main = do
    [rulesStr, updatesStr] <- splitOn [] . lines <$> readFile "../input.txt"
    let rules = map (map read . splitOn '|') rulesStr :: [[Int]]
    let updates = map (map read . splitOn ',') updatesStr :: [[Int]]
    let (correctOrder, incorrectOrder) = partition (\u -> all (`checkRule` u) rules) updates
    print $ sum $ map getMiddle correctOrder

    let corrected = map (sortBy (pageOrdering rules)) incorrectOrder
    print $ map (\u -> all (`checkRule` u) rules) corrected
    print $ sum $ map getMiddle corrected
