module Pangram
  ( isPangram
  )
where

import           Control.Monad.State
import           Data.Char                      ( toLower )
import           Data.List                      ( nub )

type PangramValue = Int
type PangramState = (String, PangramValue)

isPangram :: String -> Bool
isPangram = isPangramWithNub

-- Implementation 1: with lists
isPangramWithNub = check . length . take 26 . nub . filter isAz . map toLower

-- Implementation 2: with recursion
isPangramRecursive = flip isPangramRecursive' initalState
isPangramRecursive' [] (_, count) = check count
isPangramRecursive' (x : xs) (letters, count)
  | check count = True
  | isAz x' && (x' `notElem` letters) = isPangramRecursive'
    xs
    (x' : letters, count + 1)
  | otherwise = isPangramRecursive' xs (letters, count)
  where x' = toLower x

-- Implementation 3: with State monad
isPangramMonadic text = check $ evalState (go text) initalState
 where
  go :: String -> State PangramState PangramValue
  go []       = gets snd
  go (x : xs) = do
    letters <- gets fst
    when (isAz x' && (x' `notElem` letters))
         (modify (\(l, c) -> (x' : l, c + 1)))
    count <- gets snd
    if check count then go [] else go xs
    where x' = toLower x

initalState :: PangramState
initalState = ("", 0)

check :: PangramValue -> Bool
check = (== 26)

isAz :: Char -> Bool
isAz = (`elem` ['a' .. 'z'])
