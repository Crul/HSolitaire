{-# OPTIONS_GHC -Wall #-}
module UxState (showState) where

import Data.Text (Text, pack, null)
import Data.List (intercalate)

import qualified Crosscutting as CC
import State   (State(..))
import UxDecks (showLoopDecks, showSuitDecks, showColumnDecks)

showState :: State -> Text
showState stt = output
    where
      output = foldr append null [cls, mainSc, newLn, msgs, newLn]

      loopDs = loopDecks stt
      suitDs = suitDecks stt
      colsDs = columnDecks stt
      messgs = messages stt

      newLn  = pack "\n" -- TODO 3 from system environment
      cls    = pack "\ESC[2J"
      mainSc = intercalate "\n" (CC.columnLayout [(14, left), (3, []), (0, right)])
      msgs   = concat $ map showMessage messgs

      left   = (showLoopDecks loopDs) ++ (showSuitDecks suitDs)
      right  = showColumnDecks colsDs

showMessage :: Text -> Text
showMessage msg = (pack "\n\ESC[107;31m") ++ msg ++ (pack "\ESC[0m")

