{-# OPTIONS_GHC -Wall #-}
module UxState (showState) where

import Data.List (intercalate)

import qualified Crosscutting as CC
import Decks   (LoopDecks, SuitDecks, ColumnDecks)
import UxDecks (showLoopDecks, showSuitDecks, showColumnDecks)

showState :: LoopDecks -> SuitDecks -> ColumnDecks -> [String] -> String
showState loopDs suitDs colsDs messgs = output
    where
      output = cls ++ mainSc ++ "\n" ++ msgs

      cls    = "\ESC[2J"
      mainSc = intercalate "\n" (CC.columnLayout [(14, left), (3, []), (0, right)])
      msgs   = concat $ map showMessage messgs

      left   = (showLoopDecks loopDs) ++ (showSuitDecks suitDs)
      right  = showColumnDecks colsDs

showMessage :: String -> String
showMessage msg = "\n\ESC[107;31m" ++ msg ++ "\ESC[0m"

