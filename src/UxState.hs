{-# OPTIONS_GHC -Wall #-}
module UxState (showState) where

import Data.List (intercalate)

import qualified Crosscutting as CC
import State   (State(..))
import UxDecks (showLoopDecks, showSuitDecks, showColumnDecks)

showState :: State -> String
showState stt = output
    where
      output = cls ++ mainSc ++ "\n" ++ msgs ++ "\n"

      loopDs = loopDecks stt
      suitDs = suitDecks stt
      colsDs = columnDecks stt
      messgs = messages stt

      cls    = "\ESC[2J"
      mainSc = intercalate "\n" (CC.columnLayout [(14, left), (3, []), (0, right)])
      msgs   = concat $ map showMessage messgs

      left   = (showLoopDecks loopDs) ++ (showSuitDecks suitDs)
      right  = showColumnDecks colsDs

showMessage :: String -> String
showMessage msg = "\n\ESC[107;31m" ++ msg ++ "\ESC[0m"

