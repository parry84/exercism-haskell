module Queens
  ( boardString
  , canAttack
  )
where

import           Control.Monad                  ( guard )
import           Data.Ix                        ( range )
import           Data.List.Split                ( chunksOf )
import           Data.List                      ( intersperse )

type Position = (Int, Int)
type Board = String

boardString :: Maybe Position -> Maybe Position -> Board
boardString white black = format $ annotate <$> range ((0, 0), (7, 7))
 where
  annotate :: Position -> Char
  annotate x | white == Just x = 'W'
             | black == Just x = 'B'
             | otherwise       = '_'
  format :: String -> Board
  format = unlines . map (intersperse ' ') . chunksOf 8

canAttack :: Position -> Position -> Bool
canAttack queenA queenB = queenA `elem` moveQueen queenB
 where
  moveQueen :: Position -> [Position]
  moveQueen (c, r) = do
    (c', r') <- concat
      [ [(c + x, r + x), (c - x, r + x), (c + x, r), (c, r + x)]
      | x <- [-7 .. 7]
      ]
    guard (c' `elem` [0 .. 7] && r' `elem` [0 .. 7])
    return (c', r')
