import Data.List (isSuffixOf)
import Data.Maybe (catMaybes, isJust)

type Input = [(Int, [Int])]

splitOn :: (Eq a) => a -> [a] -> ([a], [a])
splitOn = splitOn' []
  where
    splitOn' :: (Eq a) => [a] -> a -> [a] -> ([a], [a])
    splitOn' acc _ [] = (acc, [])
    splitOn' acc c (x : xs)
        | c == x = (reverse acc, xs)
        | otherwise = splitOn' (x : acc) c xs

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

check1 :: Int -> [Int] -> Bool
check1 0 [] = True
check1 _ [] = False
check1 y (x : xs)
    | y < 0 = False
    | otherwise = (y `mod` x == 0 && check1 (y `div` x) xs) || check1 (y - x) xs

check2 :: Int -> [Int] -> Bool
check2 0 [] = True
check2 _ [] = False
check2 y (x : xs)
    | y < 0 = False
    | otherwise =
        (y `mod` x == 0 && check2 (y `div` x) xs)
            || check2 (y - x) xs
            || ( (show x `isSuffixOf` show y)
                    && check2 (read $ reverse $ drop (length $ show x) $ reverse $ show y) xs
               )

readInput :: String -> IO Input
readInput filename =
    let
        parseLine = mapFst read . mapSnd (map read . words) . splitOn ':'
     in
        map parseLine . lines <$> readFile filename

part1 :: Input -> Int
part1 input = sum $ map fst $ filter (uncurry check1 . mapSnd reverse) input

part2 :: Input -> Int
part2 input = sum $ map fst $ filter (uncurry check2 . mapSnd reverse) input

main :: IO ()
main = do
    input <- readInput "../input.txt"
    print $ part1 input
    print $ part2 input
