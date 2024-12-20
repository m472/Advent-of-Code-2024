import Data.List (nub)

doubleDist :: (Int, Int) -> (Int, Int) -> (Int, Int)
doubleDist (x1, y1) (x2, y2) = (x1 + 2 * (x2 - x1), y1 + 2 * (y2 - y1))

simplify :: (Int, Int) -> (Int, Int)
simplify (a, b)
    | a == 0 && b == 0 = undefined
    | a == 0 = (0, 1)
    | b == 0 = (1, 0)
    | otherwise =
        let
            n = gcd a b
         in
            (a `div` n, b `div` n)

calculateAntinodesPart1 :: [(Char, (Int, Int))] -> (Int, Int) -> [(Int, Int)]
calculateAntinodesPart1 antennas (rows, cols) =
    [ pos
    | f <- freqs
    , let fAntennaPositions = map snd $ filter ((== f) . fst) antennas
    , i <- [1 .. rows]
    , j <- [1 .. cols]
    , let pos = (i, j)
    , posA <- fAntennaPositions
    , pos /= posA
    , let posB = doubleDist pos posA
    , posB `elem` fAntennaPositions
    ]
  where
    freqs = nub $ map fst antennas

isInLineWithTwoAntennas :: [(Int, Int)] -> (Int, Int) -> Bool
isInLineWithTwoAntennas antennas (x, y) =
    or
        [ check (x - x1, y - y1) (dx, dy)
        | (x1, y1) <- antennas
        , (x2, y2) <- antennas
        , not (x1 == x2 && y1 == y2 && x == x1 && y == y1)
        , let dx' = x2 - x1
        , let dy' = y2 - y1
        , dx' /= 0 || dy' /= 0
        , let (dx, dy) = simplify (dx', dy')
        ]
  where
    check (0, 0) (0, 0) = undefined
    check (_, 0) (_, 0) = True
    check (0, _) (0, _) = True
    check (x, y) (dx, dy) =
        (x `mod` dx == 0)
            && (y `mod` dy == 0)
            && (x `div` dx == y `div` dy)

calculateAntinodesPart2 :: [(Char, (Int, Int))] -> (Int, Int) -> [(Int, Int)]
calculateAntinodesPart2 antennas (rows, cols) =
    [ pos
    | f <- freqs
    , let fAntennaPositions = map snd $ filter ((== f) . fst) antennas
    , i <- [1 .. rows]
    , j <- [1 .. cols]
    , let pos = (i, j)
    , isInLineWithTwoAntennas fAntennaPositions pos
    ]
  where
    freqs = nub $ map fst antennas

disp :: [(Int, Int)] -> (Int, Int) -> String
disp antinodes (rows, cols) =
    unlines
        [ [ if pos `elem` antinodes then '#' else '.'
          | i <- [1 .. cols]
          , let pos = (i, j)
          ]
        | j <- [1 .. rows]
        ]

main :: IO ()
main = do
    content <- readFile "../input.txt"
    let rows = length $ lines content
    let cols = length $ head $ lines content
    let range = (rows, cols)
    let antennas =
            [ (chr, (i, j))
            | (line, j) <- zip (lines content) [1 ..]
            , (chr, i) <- zip line [1 ..]
            , chr `notElem` ".\n"
            ]

    print $ length $ nub $ calculateAntinodesPart1 antennas range
    -- putStrLn $ flip disp range $ nub $ calculateAntinodesPart2 (filter ((== 'A') . fst) antennas) range
    print $ length $ nub $ calculateAntinodesPart2 antennas range
