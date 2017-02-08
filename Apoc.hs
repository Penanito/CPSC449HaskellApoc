{- |
Module      : Main
Description : Template to get you started on the CPSC 449 Winter 2016 Apocalypse assignment.
Copyright   : Copyright 2016, Rob Kremer (rkremer@ucalgary.ca), University of Calgary.
License     : Permission to use, copy, modify, distribute and sell this software
              and its documentation for any purpose is hereby granted without fee, provided
              that the above copyright notice appear in all copies and that both that
              copyright notice and this permission notice appear in supporting
              documentation. The University of Calgary makes no representations about the
              suitability of this software for any purpose. It is provided "as is" without
              express or implied warranty.
Maintainer  : rkremer@ucalgary.ca
Stability   : experimental
Portability : ghc 7.10.2 - 7.10.3

This module is used for CPSC 449 for the Apocalypse assignment.

Feel free to modify this file as you see fit.

-}

module Main (
      -- * Main
      main, main',
      -- * Utility functions
      replace, replace2
      ) where

import Data.Maybe (fromJust, isNothing)
import System.Environment
import System.IO.Unsafe
import ApocTools
import ApocStrategyHuman


---Main-------------------------------------------------------------

-- | The main entry, which just calls 'main'' with the command line arguments.
main = main' (unsafePerformIO getArgs)

{- | We have a main' IO function so that we can either:

     1. call our program from GHCi in the usual way
     2. run from the command line by calling this function with the value from (getArgs)
-}
main'           :: [String] -> IO()
main' args = do
    putStrLn "\nThe initial board:"
    print initBoard

    putStrLn $ "\nThe initial board with back human (the placeholder for human) strategy having played one move\n"
               ++ "(clearly illegal as we must play in rounds!):"
    move <- human (initBoard) Normal Black
    putStrLn (show $ GameState (if move==Nothing
                                then Passed
                                else Played (head (fromJust move), head (tail (fromJust move))))
                               (blackPen initBoard)
                               (Passed)
                               (whitePen initBoard)
                               (replace2 (replace2 (theBoard initBoard)
                                                   ((fromJust move) !! 1)
                                                   (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                         ((fromJust move) !! 0)
                                         E))
    newBoard <- return (GameState (if move==Nothing
                                then Passed
                                else Played (head (fromJust move), head (tail (fromJust move))))
                               (blackPen initBoard)
                               (Passed)
                               (whitePen initBoard)
                               (replace2 (replace2 (theBoard initBoard)
                                                   ((fromJust move) !! 1)
                                                   (getFromBoard (theBoard initBoard) ((fromJust move) !! 0)))
                                         ((fromJust move) !! 0)
                                         E))
   
    startNextTurn newBoard



startNextTurn                       :: GameState -> IO ()
startNextTurn boardState = do
             
--Apply strategies
  blackmove <- human (boardState) Normal Black
  
  whitemove <- return (Just [(4,4), (3,3)])
  
  newBoard <- return (GameState (if blackmove==Nothing then Passed else Played (head (fromJust blackmove), head (tail (fromJust blackmove))))
                     (blackPen boardState)
                     (if whitemove==Nothing then Passed else Played (head (fromJust whitemove), head (tail (fromJust whitemove))))
                     (whitePen boardState)
                     (replace2 (replace2 (theBoard boardState)
                                                   ((fromJust blackmove) !! 1)
                                                   (getFromBoard (theBoard boardState) ((fromJust blackmove) !! 0)))
                                         ((fromJust blackmove) !! 0)
                                         E))
                     
  
  putStrLn (show $ newBoard)
  
  startNextTurn newBoard

getInput :: IO String
getInput = getLine 

--Could also GameState -> Bool, and extract data from the structure that is GameState	
--Basically the @ function assigns a name to a specific part of a bigger data type
--as below player is the name, whitePlayed is the data type inside GameState
--So, we're extracting the data from whitePlayed which is type Played and we only care about 
--the second tuple in the pair of tuples, and thus have named it s@ second tuble values
--It's unfinished at the moment as I wasn't sure of the format we as a group wanted to follow.
--Basically, options are GameState ->Bool, and extract the data, or
--GameState -> PlayType -> Player -> Played -> Bool
--I'm leaning extracting the data though, as that makes it more useful.
isValid :: GameState -> PlayType -> Player -> Played -> Bool
isValid GameState{theBoard = nBoard, whitePlayed = player@(Played (_, s@(d,3)) } Normal White move =


---2D list utility functions-------------------------------------------------------

-- | Replaces the nth element in a row with a new element.
replace         :: [a] -> Int -> a -> [a]
replace xs n elem = let (ys,zs) = splitAt n xs
                     in (if null zs then (if null ys then [] else init ys) else ys)
                        ++ [elem]
                        ++ (if null zs then [] else tail zs)

-- | Replaces the (x,y)th element in a list of lists with a new element.
replace2        :: [[a]] -> (Int,Int) -> a -> [[a]]
replace2 xs (x,y) elem = replace xs y (replace (xs !! y) x elem)

---------------------functions----------------------------
-------------------------------------------------------------------
-- 0? no one won, 1? White won, 2? Black won
checkGameOver :: GameState -> Int
checkGameOver g
    | countPieceBoard (theBoard g) WP == 0 && countPieceBoard (theBoard g) BP == 0 = 0
    | countPieceBoard (theBoard g) WP == 0 = 2
    | countPieceBoard (theBoard g) BP == 0 = 1
    | noMovesLeft (theBoard g) == True = 0
    | otherwise = 0
    
    
-----------------------Returns whether or not there are any permissible moves left in the board
noMovesLeft :: Board -> Bool
noMovesLeft b
    | hasMoves b White == False && hasMoves b Black == False = True
    | otherwise = False
    
hasMoves :: Board -> Player -> Bool
hasMoves b White = False
hasMoves b Black = False
-----------------------Are there [certain piece] left? Returns the number of said piece -----------
countPieceBoard :: Board -> Cell -> Int
countPieceBoard [] _ = 0
countPieceBoard (r:rs) t = countPieceRow r t + countPieceBoard rs t

countPieceRow :: [Cell] -> Cell -> Int
countPieceRow [] _ = 0
countPieceRow (c:cs) t
    | c == t = 1 + countPieceRow cs t
    | otherwise = 0 + countPieceRow cs t
    
---- given a cell, whether or not it can eat something, if it can, returns the cell it can eat 
--canEatSomething
    -- | 

--handles all pawn placement logic and implementation for a given player
handlePromotionAndPawnPlacement ::  GameState -> Player -> GameState
handlePromotionAndPawnPlacement g White
    | hasPawnReachedEnd g White == False = g
    | canPromote g White == True = promotePawn g White
    | otherwise = placePawn g White
handlePromotionAndPawnPlacement g Black
    | hasPawnReachedEnd g Black == False = g
    | canPromote g Black == True = promotePawn g Black
    | otherwise = placePawn g Black
    
hasPawnReachedEnd :: GameState -> Player -> Bool
hasPawnReachedEnd g White
    | countPieceRow (last (theBoard g)) WP == 0 = False
    | otherwise = True
hasPawnReachedEnd g Black
    | countPieceRow (head (theBoard g)) BP == 0 = False
    | otherwise = True
    
canPromote :: GameState -> Player -> Bool
canPromote g White
    | countPieceBoard (theBoard g) WK < 2 = True
    | otherwise = False
canPromote g Black
    | countPieceBoard (theBoard g) BK < 2 = True
    | otherwise = False
    
promotePawn :: GameState -> Player -> GameState
promotePawn g White = initBoard-- We know how to get the (Int, Int) of the knight but how to replace it in the board?
promotePawn g Black = initBoard-- ^

placePawn :: GameState -> Player -> GameState
placePawn _ _ = initBoard -- We know how to get the (Int, Int), easy to check for invalid placement, but how to replace and return board?
    
getPieceRow :: [Cell] -> Cell -> Int
getPieceRow [] _ = 0
getPieceRow (c:cs) t
    | c == t = 0
    | otherwise = 1 + getPieceRow cs t
----

