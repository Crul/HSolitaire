{-# OPTIONS_GHC -Wall #-}
module State where

import qualified Data.Vector as V

import qualified Crosscutting as CC
import Decks   (Deck, LoopDecks, SuitDecks, ColumnDeck, ColumnDecks, columnDeck, setColumnDeck)
import UxState (showState)  -- TODO 2 Invert Dependency

data State = State { loopDecks   :: LoopDecks
                   , suitDecks   :: SuitDecks
                   , columnDecks :: ColumnDecks
                   , messages    :: [String]
                   , previous    :: Maybe State
-- TODO 4 time     , startTime   :: ???
-- TODO 4 time     , pausesSwts  :: [???]
                   }

instance Show State where
  show stt = showState (loopDecks stt) (suitDecks stt) (columnDecks stt) (messages stt)

addMessage :: State -> String -> State
addMessage st msg = st { messages = (messages st) ++ [msg]}

setLoop :: State -> Deck -> Deck -> State
setLoop stt hLD vLD = stt { loopDecks = (hLD, vLD) }

win :: State -> Bool
win State { suitDecks = (spD, hrD, clD, diD) } =
    all (==13) $ map V.length [spD, hrD, clD, diD]

previousOrCurrent :: State -> State
previousOrCurrent stt = CC.maybeOr (previous stt)
                            {-or-} (addMessage stt "No more undo is possible")

stColumnDeck :: State -> Int -> ColumnDeck
stColumnDeck stt c = columnDeck (columnDecks stt) c

setStColumnDeck :: State -> Int -> ColumnDeck -> State
setStColumnDeck stt c cD = stt { columnDecks = setColumnDeck (columnDecks stt) c cD }

setStColumnDecks :: State -> [(Int, ColumnDeck)] -> State
setStColumnDecks stt changes = stt { columnDecks = foldr setColumnDeck' (columnDecks stt) changes }
    where setColumnDeck' (c,cD) colDs = setColumnDeck colDs c cD
