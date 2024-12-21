import Data.Array
import Data.List (singleton)

data Block
    = Empty
    | File Int
    deriving (Show, Eq)

expand :: [Int] -> [Block]
expand = expand' 0
  where
    expand' _ [] = []
    expand' nextId [fileSize] = replicate fileSize (File nextId)
    expand' nextId (fileSize : emptySize : remainder) = replicate fileSize (File nextId) ++ replicate emptySize Empty ++ expand' (nextId + 1) remainder

compact :: [Block] -> [Block]
compact blocks = elems $ compact' 1 len (listArray (1, len) blocks)
  where
    len = length blocks
    compact' :: Int -> Int -> Array Int Block -> Array Int Block
    compact' left right blocks
        | left >= right = blocks
        | l == Empty && r /= Empty = compact' (left + 1) (right - 1) $ blocks // [(left, r), (right, l)]
        | otherwise = compact' (if l == Empty then left else left + 1) (if r /= Empty then right else right - 1) blocks
      where
        l = blocks ! left
        r = blocks ! right

checksum :: [Block] -> Int
checksum blocks = sum $ zipWith (*) (map fileId blocks) [0 ..]
  where
    fileId Empty = 0
    fileId (File n) = n

main :: IO ()
main = do
    content <- map (read . singleton) . filter (/= '\n') <$> readFile "../input.txt" :: IO [Int]
    print $ checksum $ compact $ expand content
