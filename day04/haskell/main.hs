import Data.List (transpose)
import Debug.Trace (traceShowId)

countXMAS :: String -> Int
countXMAS [] = 0
countXMAS ('X' : 'M' : 'A' : 'S' : cs) = 1 + countXMAS cs
countXMAS (c : cs) = countXMAS cs

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal grid = case head grid of
    [] -> []
    cs -> head cs : diagonal (map tail $ tail grid)

diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals grid = diagonalsLower grid ++ tail (diagonalsUpper grid)
  where
    diagonalsLower :: [[a]] -> [[a]]
    diagonalsLower [] = []
    diagonalsLower grid = diagonal grid : diagonalsLower (tail grid)

    diagonalsUpper :: [[a]] -> [[a]]
    diagonalsUpper [] = []
    diagonalsUpper grid = case diagonal grid of
        [] -> []
        diag -> diag : diagonalsUpper (map tail grid)

main :: IO ()
main = do
    content <- lines <$> readFile "../example.txt"
    let transforms =
            [ id
            , reverse
            , transpose
            , reverse . transpose
            , diagonals
            , diagonals . transpose
            , reverse . diagonals
            , reverse . diagonals . transpose
            ] ::
                [[[a]] -> [[a]]]

    putStrLn $ unlines content
    putStrLn $ unlines $ map (unlines . (\t -> t content)) transforms

-- print (diag + diagT + diagR + diagRT + hor + horR + vert + vertR)
