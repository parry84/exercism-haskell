module Pangram
  ( isPangram
  )
where

import           Control.Monad.State
import           Data.Char

type PangramValue = Int
type PangramState = (String, PangramValue)

isPangram :: String -> Bool
isPangram text = isPangramRecursive text initalState

isPangramRecursive [] (_, count) = check count
isPangramRecursive (x : xs) (letters, count)
  | check count = True
  | isAlpha x && isAscii x && (x' `notElem` letters) = isPangramRecursive
    xs
    (x' : letters, count + 1)
  | otherwise = isPangramRecursive xs (letters, count)
  where x' = toLower x

isPangramMonadic :: String -> Bool
isPangramMonadic text = check $ evalState (go text) initalState
 where
  go :: String -> State PangramState PangramValue
  go []       = gets snd
  go (x : xs) = do
    letters <- gets fst
    when (isAlpha x && isAscii x && (x' `notElem` letters))
         (modify (\(l, c) -> (x' : l, c + 1)))
    count <- gets snd
    if check count then go [] else go xs
    where x' = toLower x

initalState :: PangramState
initalState = ("", 0)

check :: PangramValue -> Bool
check = (== 26)
