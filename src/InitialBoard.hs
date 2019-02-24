{-# OPTIONS_GHC -Wall #-}
module InitialBoard (initialBoard) where

import qualified Data.Vector as V

import Decks (Deck, moveMaxCards)
import State (State(..))

initialBoard :: Deck -> State
initialBoard deck = State { loopDecks   = lpDecks
                          , suitDecks   = stDecks
                          , columnDecks = ( (hC0D, vC0D)
                                          , (hC1D, vC1D)
                                          , (hC2D, vC2D)
                                          , (hC3D, vC3D)
                                          , (hC4D, vC4D)
                                          , (hC5D, vC5D)
                                          , (hC6D, vC6D)
                                          )
                          , messages    = msgs
                          , previous    = Nothing
                          , autoSolving = False
                          }
  where
    lpDecks = (lpDeck, V.empty)
    stDecks = (V.empty, V.empty, V.empty, V.empty)
    msgs    = []

    hC0D = V.empty
    (Just (deck1,  hC1D)) = moveMaxCards 1 (deck  , V.empty)
    (Just (deck2,  hC2D)) = moveMaxCards 2 (deck1 , V.empty)
    (Just (deck3,  hC3D)) = moveMaxCards 3 (deck2 , V.empty)
    (Just (deck4,  hC4D)) = moveMaxCards 4 (deck3 , V.empty)
    (Just (deck5,  hC5D)) = moveMaxCards 5 (deck4 , V.empty)
    (Just (deck6,  hC6D)) = moveMaxCards 6 (deck5 , V.empty)

    (Just (deck7,  vC0D)) = moveMaxCards 1 (deck6 , V.empty)
    (Just (deck8,  vC1D)) = moveMaxCards 1 (deck7 , V.empty)
    (Just (deck9,  vC2D)) = moveMaxCards 1 (deck8 , V.empty)
    (Just (deck10, vC3D)) = moveMaxCards 1 (deck9 , V.empty)
    (Just (deck11, vC4D)) = moveMaxCards 1 (deck10, V.empty)
    (Just (deck12, vC5D)) = moveMaxCards 1 (deck11, V.empty)
    (Just (deck13, vC6D)) = moveMaxCards 1 (deck12, V.empty)

    lpDeck = deck13
