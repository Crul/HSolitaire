{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module Engine (run) where

import Control.Monad.Random

import Decks        (newDeck)
import State        (State, addMessage, win)
import InitialBoard (initialBoard)
import Commands     (executeCmd)

play :: State -> IO ()
play stt = if (win stt) then won else  play'
  where
    play' = print stt
          >> getLine
          >>= executeCmd run play stt

    won   = print (addMessage stt "You won!")
          >> getLine
          >> run  -- TODO 5 win animation


run :: IO ()
run = do
  deck <- evalRandIO newDeck
  play $ initialBoard deck
