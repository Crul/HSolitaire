{-# OPTIONS_GHC -Wall #-}
module Commands (executeCmd) where

import Data.List (intercalate)

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
        "help"  -> showHelp
        "h"     -> showHelp
        "?"     -> showHelp
        "-"     -> undo  -- TODO 3 multiple "-" should undo multiple times
        "u"     -> undo
        "undo"  -> undo
        "*"     -> autoSolve
        "+"     -> autoSolve  -- TODO 3 autoSolve single step
        _       -> execAct
    where
      exit      = putStrLn "Goodbye"
      undo      = play $ (previousOrCurrent stt) { autoSolving = False }
      autoSolve = play $ stt { autoSolving = True }

      cleanStt  = stt { messages=[] }
      execAct   = play nextState
        where
          actedState = action cleanStt act
          nextState  = setPrev actedState      -- TODO 1 move previous to avoid undo nothing when no actions
      
      setPrev s = s { previous = Just stt }

      showHelp  = play $ stt { messages=[help] }


help :: String
help = intercalate "\n" [
    "HSolitaire Help"
  , "---------------"
  , ""
  , "Controls:      "
  , " ENTER         Reveals next 3 cards in the Main Deck  "
  , " .             Move the top card in the Main Deck     "
  , "               to the Final Decks                     "
  , " [0..6]        Move the top card in one of the        "
  , "               Column Decks to the Final Decks        "
  , " [0..6][0..6]  Move cards in one of the Column Decks  "
  , "               to other Column Decks                  "
  , "                 e.g.: `40` Moves cards from          "
  , "                            column 4 to column 0      "
  , " .[0..6]       Move the top card in the Main Deck     "
  , "               to a Column Decks                      "
  , "                 e.g.: `.6` Moves the top card in the "
  , "                            Main Deck to the column 6 "
  , " [0..4]/[0..6] Move one card from a Final Deck to a   "
  , "               Column Deck                            "
  , "                 e.g.: `3/2` Moves the top card in    "
  , "                             the Final Deck 3 to      "
  , "                             the column 2             "
  , " +             Move all positionable cards to the     "
  , "               Final Decks (to solve when trivial)    "
  , " *             Same as + (auto trivial solving)       "
  , ""
  , "Other commands:"
  , " quit  | q | exit   Exit the game              "
  , " undo  | u | -      Undo last move             "
  , " reset | r          Restart game with new deck "
  , " help  | h | ?      Show this help             "
  ]
