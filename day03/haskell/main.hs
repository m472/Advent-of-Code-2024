{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Data.Bifunctor qualified as Bifunctor
import Data.Char (isDigit)
import Data.Functor
import Data.Maybe (catMaybes)

data Instruction
    = Mul Int Int
    | Do
    | Dont
    deriving (Show)

newtype Parser a = Parser {parse :: String -> [(a, String)]}

instance Functor Parser where
    fmap f (Parser p) = Parser (map (Bifunctor.first f) . p)

instance Applicative Parser where
    pure x = Parser (\cs -> [(x, cs)])
    f <*> a =
        Parser
            ( \cs ->
                concat [parse (fmap fn a) cs' | (fn, cs') <- parse f cs]
            )

instance Alternative Parser where
    empty = Parser (const [])
    p <|> q =
        Parser
            ( \cs ->
                let (p', q') = (parse p cs, parse q cs)
                 in if not (null p') then p' else q'
            )

instance Monad Parser where
    return = pure
    p >>= f =
        Parser
            ( \cs ->
                concat [parse (f a) cs' | (a, cs') <- parse p cs]
            )

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate =
    Parser
        ( \case
            (c : cs) -> [(c, cs) | predicate c]
            _ -> []
        )

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string "" = return []
string (c : cs) = (:) <$> char c <*> string cs

num :: Parser Int
num = do
    first <- satisfy isDigit
    second <- optional $ satisfy isDigit
    third <- optional $ satisfy isDigit
    return $ read $ first : catMaybes [second] ++ catMaybes [third]

mul :: Parser Instruction
mul = do
    first <- string "mul(" *> num
    second <- string "," *> num <* string ")"
    return $ Mul first second

do' :: Parser Instruction
do' = string "do()" $> Do

don't :: Parser Instruction
don't = string "don't()" $> Dont

instruction :: Parser Instruction
instruction = mul <|> don't <|> do'

findInstructions :: String -> [Instruction]
findInstructions [] = []
findInstructions s = case parse instruction s of
    [] -> findInstructions (tail s)
    [(instr, rest)] -> instr : findInstructions rest
    _ -> undefined

executePart1 :: Instruction -> Int
executePart1 (Mul a b) = a * b
executePart1 _ = 0

executePart2 :: [Instruction] -> Int
executePart2 = fst . foldl exec (0, True)
  where
    exec :: (Int, Bool) -> Instruction -> (Int, Bool)
    exec (n, True) (Mul a b) = (n + a * b, True)
    exec (n, True) Dont = (n, False)
    exec (n, False) Do = (n, True)
    exec state _ = state

main :: IO ()
main = do
    instr <- findInstructions <$> readFile "../input.txt"
    print $ sum $ map executePart1 instr
    print $ executePart2 instr
