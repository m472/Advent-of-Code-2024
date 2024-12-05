import Data.List (transpose)
import Debug.Trace (traceShowId)

countXMAS :: String -> Int
countXMAS [] = 0
countXMAS ('X' : 'M' : 'A' : 'S' : cs) = 1 + countXMAS cs
countXMAS (c : cs) = countXMAS cs

diagonal :: (Show a) => [[a]] -> [a]
diagonal [] = []
diagonal grid = case head grid of
    [] -> []
    cs -> (head cs) : (diagonal $ map tail $ tail grid)

diagonals :: (Show a) => [[a]] -> [[a]]
diagonals [] = []
diagonals grid = diagonalsLower grid ++ tail (diagonalsUpper grid)
  where
    diagonalsLower :: (Show a) => [[a]] -> [[a]]
    diagonalsLower [] = []
    diagonalsLower grid = diagonal grid : diagonalsLower (tail grid)

    diagonalsUpper :: (Show a) => [[a]] -> [[a]]
    diagonalsUpper [] = []
    diagonalsUpper grid = case diagonal grid of
        [] -> []
        diag -> diag : diagonalsUpper (map tail grid)

main :: IO ()
main = do
    content <- lines <$> readFile "../example.txt"
    let diag = sum $ map countXMAS $ diagonals content
    let diagT = sum $ map countXMAS $ diagonals $ transpose content
    let diagR = sum $ map countXMAS $ diagonals $ reverse  content
    let diagRT = sum $ map countXMAS $ diagonals $ reverse $ transpose content
    let hor = sum $ map countXMAS $ content
    let horR = sum $ map countXMAS $ reverse content
    let vert = sum $ map countXMAS $ transpose $ content
    let vertR = sum $ map countXMAS $ reverse $ transpose content
    print (diag + diagT + diagR + diagRT + hor + horR + vert + vertR)
