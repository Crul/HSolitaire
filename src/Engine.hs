{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Engine (run) where

import Control.Monad.Random

import Decks        (newDeck)
import State        (State(..), addMessage, win)
import InitialBoard (initialBoard)
import Commands     (executeCmd)
import UxState      (showState)
import UxSplash     (splashScreen)
import Solver       (solve)

play :: State -> IO ()
play stt | win stt = showSt (addMessage stt "You won!")
                     >> getLine
                     >> run'  -- TODO 5 win animation
  
play stt | autoSolving stt = autoSolve
  where
    cleanStt  = stt { messages=[] }  -- TODO 3 DRY
    autoSolve = play $ solve cleanStt -- TODO 3 DRY -- TODO 1 move previous to avoid undo nothing when no actions>

play stt | otherwise = showSt stt
                     >> getLine
                     >>= executeCmd run' play stt


run :: IO ()
run = putStrLn splashScreen >> getLine >> run'

run' :: IO ()
run' = do  -- TODO 2 Splash Screen
  deck <- evalRandIO newDeck
  play $ initialBoard deck


showSt :: State -> IO ()
showSt = putStr . showState
