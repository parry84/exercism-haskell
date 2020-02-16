module Pangram
  ( isPangram
  , main
  )
where

import           Control.Monad.State
import           Data.Char                      ( toLower )
import           Data.List                      ( nub
                                                , intercalate
                                                )
import           Data.Set                       ( fromList
                                                , isSubsetOf
                                                , (\\)
                                                )
import           Control.Exception              ( evaluate )
import           System.Time.Extra              ( duration )

type PangramValue = Int
type PangramState = (String, PangramValue)

isPangram :: String -> Bool
isPangram = isPangramWithAll


-- Implementation 1: with sub set
isPangramWithSubset = (alphabet `isSubsetOf`) . fromList . lower

-- Implementation 2: with set difference
isPangramWithSetDiff = null . (alphabet \\) . fromList . lower

alphabet = fromList ['a' .. 'z']

-- Implementation 3: with 'all'
isPangramWithAll text = all (`elem` lower text) ['a' .. 'z']

-- Implementation 4: with 'nub'
isPangramWithNub = check . length . take 26 . nub . filter isAz . lower

-- Implementation 5: with recursion
isPangramRecursive = flip isPangramRecursive' initalState
isPangramRecursive' [] (_, count) = check count
isPangramRecursive' (x : xs) (letters, count)
  | check count = True
  | isAz x' && (x' `notElem` letters) = isPangramRecursive'
    xs
    (x' : letters, count + 1)
  | otherwise = isPangramRecursive' xs (letters, count)
  where x' = toLower x

-- Implementation 6: with State monad
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

lower :: String -> String
lower = fmap toLower

-- benchmark functions, based on code in http://neilmitchell.blogspot.com/2015/02/nub-considered-harmful.html

benchmark :: String -> IO ()
benchmark xs = do
  n       <- evaluate $ length xs
  (t1, _) <- duration $ evaluate $ isPangramWithSubset xs
  (t2, _) <- duration $ evaluate $ isPangramWithSetDiff xs
  (t3, _) <- duration $ evaluate $ isPangramWithAll xs
  (t4, _) <- duration $ evaluate $ isPangramWithNub xs
  (t5, _) <- duration $ evaluate $ isPangramRecursive xs
  (t6, _) <- duration $ evaluate $ isPangramMonadic xs
  putStrLn $ intercalate "," $ show n : fmap show [t1, t2, t3, t4, t5, t6]

main :: IO ()
main = do
  forM_ [0, 100 .. 10000] $ \i -> benchmark $ take (26 * i) $ cycle ['a' .. 'z']
  forM_ [0, 100 .. 10000] $ \i -> benchmark $ replicate i 'a'
