{-# OPTIONS_GHC -Wall #-}
module Commands where

import State   (State(..), previousOrCurrent)
import Actions (action)

executeCmd :: IO () -> (State -> IO ()) -> State -> String -> IO ()
executeCmd reset play stt act =
  case act of
    "reset" -> reset
    "r"     -> reset
    "exit"  -> exit
    "quit"  -> exit
    "q"     -> exit
    ":q"    -> exit
    "help"  -> help
    "h"     -> help
    "?"     -> help
    "-"     -> undo
    "u"     -> undo
    "undo"  -> undo
--  "*"     -> solve TODO 1
    _       -> play nextState
                where
                  cleanState = stt { messages= [] }
                  actedState = action cleanState act
                  nextState  = actedState { previous = Just stt }  -- TODO 2 move previous to avoid undo nothing when no actions
  where
    exit = putStrLn "Goodbye"
    undo = play $ previousOrCurrent stt
    help = putStrLn "HELP TODO 3"

