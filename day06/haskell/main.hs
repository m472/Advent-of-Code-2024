import Data.Array
import Data.List (nub)
import Debug.Trace (traceShow, traceShowId)

data Direction
    = Up
    | Down
    | Lft
    | Rght

type Map = Array (Int, Int) Char

parseInput :: String -> Map
parseInput content =
    array ((0, 0), (rows - 1, cols - 1)) $
        concat $
            zipWith (\i line -> zipWith (\j c -> ((i, j), c)) [0 ..] line) [0 ..] (lines content)
  where
    rows = length $ lines content
    cols = length $ head $ lines content

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = fst $ chunksOf' ([[]], n) n xs
  where
    chunksOf' :: ([[a]], Int) -> Int -> [a] -> ([[a]], Int)
    chunksOf' (acc, _) _ [] = (reverse (map reverse acc), -1)
    chunksOf' (acc, 0) n xs = chunksOf' ([] : acc, n) n xs
    chunksOf' (a : acc, i) n (x : xs) = chunksOf' ((x : a) : acc, i - 1) n xs

dispMap :: Map -> IO ()
dispMap grid = mapM_ putStrLn $ chunksOf (lastCol - firstCol + 1) $ elems grid
  where
    ((_, firstCol), (_, lastCol)) = bounds grid

coordInFront :: ((Int, Int), Direction) -> (Int, Int)
coordInFront ((x, y), dir) = (x + dx, y + dy)
  where
    (dx, dy) = case dir of
        Up -> (-1, 0)
        Down -> (1, 0)
        Lft -> (0, -1)
        Rght -> (0, 1)

turnRight :: Direction -> Direction
turnRight Up = Rght
turnRight Rght = Down
turnRight Down = Lft
turnRight Lft = Up

simulate :: ((Int, Int), Direction) -> ((Int, Int), (Int, Int)) -> [(Int, Int)] -> [(Int, Int)]
simulate (pos, dir) bounds' obstacles
    | not $ inRange bounds' cif = [pos]
    | cif `notElem` obstacles = pos : simulate (cif, dir) bounds' obstacles
    | otherwise = simulate (pos, turnRight dir) bounds' obstacles
  where
    cif = coordInFront (pos, dir)

main :: IO ()
main = do
    grid <- parseInput <$> readFile "../input.txt"
    let startPos = head $ map fst $ filter ((== '^') . snd) $ assocs grid
    let obstacles = map fst $ filter ((== '#') . snd) $ assocs grid
    print $ bounds grid
    print $ length $ nub $ simulate (startPos, Up) (bounds grid) obstacles
