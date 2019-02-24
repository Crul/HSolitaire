{-# OPTIONS_GHC -Wall #-}
module Solver (solve) where

import qualified Crosscutting as CC
import State   (State(..))
import Actions (loopToSuitDeck', colToSuitDeck')

solve :: State -> State
solve stt = setAutoSolving (CC.maybeOr solved' unsolvable)
  where
    unsolvable = stt

    solved     = CC.maybeOrMaybe tryLoopToSuit tryColumnsToSuit
    solved'    = CC.chainMaybe solved setPrev
    setPrev s  = s { previous = Just stt } -- TODO DRY

    tryLoopToSuit    = loopToSuitDeck' stt
    tryColumnsToSuit = tryColumnToSuit (Just stt) 8

    setAutoSolving s = s { autoSolving = CC.isJust solved }


tryColumnToSuit :: Maybe State -> Int -> Maybe State
tryColumnToSuit _    0 = Nothing
tryColumnToSuit mStt n = CC.maybeOrMaybe columnToSuitMoved continueTrying
  where
    n' = pred n

    columnToSuitMoved   = CC.continueMaybe {-if-} mStt {-then-} (colToSuitDeck'')
    colToSuitDeck'' stt = colToSuitDeck' stt n'

    continueTrying      = tryColumnToSuit mStt n'
