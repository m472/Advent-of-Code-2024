import Data.List (sort)

minAt :: (Ord a) => [a] -> Maybe Int
minAt xs = snd <$> minAt' Nothing (zip xs [0 ..])
  where
    minAt' :: (Ord a) => Maybe (a, Int) -> [(a, Int)] -> Maybe (a, Int)
    minAt' a [] = a
    minAt' (Nothing) ((x, _) : xs) = minAt' (Just (x, 0)) xs
    minAt' (Just (currentMin, minInd)) ((x, ind) : xs) =
        let
            newMin = if x < currentMin then (Just (x, ind)) else (Just (currentMin, minInd))
         in
            minAt' newMin xs

roll :: Int -> [a] -> [a]
roll n xs = drop n xs ++ take n xs

parse :: String -> [(Int, Int)]
parse s =
    let
        numbers = map (map read . words) $ lines s :: [[Int]]
     in
        map (\[a, b] -> (a, b)) numbers

part1 :: [(Int, Int)] -> Int
part1 numbers =
    sum $
        map abs $
            zipWith
                (-)
                (sort $ map fst numbers)
                (sort $ map snd numbers)

count :: (Eq a) => a -> [a] -> Int
count a xs = length $ filter (== a) xs

part2 :: [(Int, Int)] -> Int
part2 numbers =
    let
        left = map fst numbers
        right = map snd numbers
     in
        sum $ map (\n -> (count n right) * n) left

main :: IO ()
main = do
    content <- readFile "../input.txt"
    print $ part1 $ parse content
    print $ part2 $ parse content
