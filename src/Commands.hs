{-# OPTIONS_GHC -Wall #-}
module Commands (executeCmd) where

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
        "-"     -> undo  -- TODO 3 multiple "-" should undo multiple times
        "u"     -> undo
        "undo"  -> undo
        "*"     -> autoSolve
        "+"     -> autoSolve  -- TODO 3 autoSolve single step
        _       -> execAct
    where
      exit      = putStrLn "Goodbye"
      undo      = play $ (previousOrCurrent stt) { autoSolving = False }
      help      = putStrLn "HELP -- TODO 3 "
      autoSolve = play $ stt { autoSolving = True }

      cleanStt  = stt { messages=[] }
      execAct   = play nextState
        where
          actedState = action cleanStt act
          nextState  = setPrev actedState      -- TODO 1 move previous to avoid undo nothing when no actions
      
      setPrev s = s { previous = Just stt }