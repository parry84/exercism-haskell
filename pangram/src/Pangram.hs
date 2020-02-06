module Pangram
  ( isPangram
  )
where

import           Control.Monad.State
import           Data.Char

type PangramValue = Int
type PangramState = (String, PangramValue)

isPangram :: String -> Bool
isPangram text = check $ evalState (go text) initalState
 where
  initalState :: PangramState
  initalState = ("", 0)

  check :: PangramValue -> Bool
  check = (== 26)

  go :: String -> State PangramState PangramValue
  go []       = gets snd
  go (x : xs) = do
    letters <- gets fst
    when (isAlpha x && isAscii x && (x' `notElem` letters))
         (modify (\(l, c) -> (x' : l, c + 1)))
    count <- gets snd
    if check count then go [] else go xs
    where x' = toLower x
