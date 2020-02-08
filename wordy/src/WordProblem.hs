module WordProblem
  ( answer
  )
where

import           Text.ParserCombinators.ReadP
import           Control.Applicative            ( (<|>) )

data Op a = Plus a | Minus a | By a | DivBy a

answer :: String -> Maybe Integer
answer problem = case readP_to_S parseProblem problem of
  [(x, "")] -> Just x
  _         -> Nothing

parseProblem :: ReadP Integer
parseProblem = do
  _   <- string "What is "
  n   <- parseNumber
  ops <- many (parseSum <|> parseSub <|> parseMult <|> parseDiv)
  _   <- string "?"
  let eval = foldl (flip opToFunction) n ops
  return eval

parseSum :: ReadP (Op Integer)
parseSum = do
  _ <- string " plus "
  Plus <$> parseNumber

parseSub :: ReadP (Op Integer)
parseSub = do
  _ <- string " minus "
  Minus <$> parseNumber

parseMult :: ReadP (Op Integer)
parseMult = do
  _ <- string " multiplied by "
  By <$> parseNumber

parseDiv :: ReadP (Op Integer)
parseDiv = do
  _ <- string " divided by "
  DivBy <$> parseNumber

parseNumber :: ReadP Integer
parseNumber = readS_to_P reads

opToFunction :: Op Integer -> Integer -> Integer
opToFunction op = case op of
  Plus  x -> (+ x)
  Minus x -> subtract x
  By    x -> (* x)
  DivBy x -> flip div x
