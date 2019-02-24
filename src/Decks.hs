{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wall #-}
module Decks ( Deck
             , LoopDecks
             , SuitDecks
             , ColumnDeck
             , ColumnDecks
             , NextCardResult
             , suitDeckIdx
             , suitDeck
             , setSuitDeck
             , columnDeck
             , setColumnDeck
             , newDeck
             , nextCard
             , moveCard
             , moveMaxCards) where

import Control.Monad.Random
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

import qualified Crosscutting as CC
import Cards (Card(..), Suit(..), labels, suits)

type Rnd a = Rand StdGen a

type Deck = Vector Card

type LoopDecks   = (Deck, Deck)              -- Hidden, Visible
type SuitDecks   = (Deck, Deck, Deck, Deck)  -- Spade, Heart, Club, Diamond
type ColumnDeck  = (Deck, Deck)              -- Hidden, Visible
type ColumnDecks = (ColumnDeck, ColumnDeck, ColumnDeck, ColumnDeck, ColumnDeck, ColumnDeck, ColumnDeck)

type NextCardResult = (Card, Deck)


suitDeckIdx :: Card -> Int
suitDeckIdx (Card _ Spade   ) = 0
suitDeckIdx (Card _ Heart   ) = 1
suitDeckIdx (Card _ Club    ) = 2
suitDeckIdx (Card _ Diamond ) = 3

suitDeck :: SuitDecks -> Int -> Deck
suitDeck (sD,_,_,_) 1 = sD
suitDeck (_,sD,_,_) 2 = sD
suitDeck (_,_,sD,_) 3 = sD
suitDeck (_,_,_,sD) 4 = sD
suitDeck _          _ = V.empty  -- TODO 5 ??

setSuitDeck :: SuitDecks -> Int -> Deck -> SuitDecks
setSuitDeck (sD1,sD2,sD3,sD4) n sD = case n of
    1 -> (sD,sD2,sD3,sD4)
    2 -> (sD1,sD,sD3,sD4)
    3 -> (sD1,sD2,sD,sD4)
    4 -> (sD1,sD2,sD3,sD)
    _ -> (sD1,sD2,sD3,sD4)


columnDeck :: ColumnDecks -> Int -> ColumnDeck
columnDeck (cD,_,_,_,_,_,_) 1 = cD
columnDeck (_,cD,_,_,_,_,_) 2 = cD
columnDeck (_,_,cD,_,_,_,_) 3 = cD
columnDeck (_,_,_,cD,_,_,_) 4 = cD
columnDeck (_,_,_,_,cD,_,_) 5 = cD
columnDeck (_,_,_,_,_,cD,_) 6 = cD
columnDeck (_,_,_,_,_,_,cD) 7 = cD
columnDeck _                _ = (V.empty, V.empty)  -- TODO 5 ??

setColumnDeck :: ColumnDecks -> Int -> ColumnDeck -> ColumnDecks
setColumnDeck (cD1,cD2,cD3,cD4,cD5,cD6,cD7) n cD = case n of
    1 -> (cD,cD2,cD3,cD4,cD5,cD6,cD7)
    2 -> (cD1,cD,cD3,cD4,cD5,cD6,cD7)
    3 -> (cD1,cD2,cD,cD4,cD5,cD6,cD7)
    4 -> (cD1,cD2,cD3,cD,cD5,cD6,cD7)
    5 -> (cD1,cD2,cD3,cD4,cD,cD6,cD7)
    6 -> (cD1,cD2,cD3,cD4,cD5,cD,cD7)
    7 -> (cD1,cD2,cD3,cD4,cD5,cD6,cD)
    _ -> (cD1,cD2,cD3,cD4,cD5,cD6,cD7)


allCards :: Deck
allCards = [ Card lb st | lb <- labels, st <- suits ]

newDeck :: Rnd Deck
newDeck = shuffle allCards

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = shuffle' (length vec) vec

shuffle' :: Int -> Vector a -> Rnd (Vector a)
shuffle' 0 vec = return vec
shuffle' i vec = do j <- getRandomR (0, i')
                    let vec' = vec // [(i', vec!j), (j, vec!i')]
                    shuffle' i' vec'
                 where i' = pred i


nextCard :: Deck -> Maybe NextCardResult
nextCard deck = CC.maybeIfVal {-if-} (not $ V.null deck)
                              {-else-} (V.head deck, V.tail deck)


moveCard :: Deck -> Deck -> Maybe (Deck, Deck)
moveCard fr to = CC.chainMaybe {-if-} (nextCard fr) {-then-} movedDecks
  where movedDecks (crd, fr') = (fr', V.cons crd to)


moveMaxCards :: Int -> (Deck, Deck) -> Maybe (Deck, Deck)
moveMaxCards n (frDeck,toDeck) = CC.maybeIf (n > 0) start
  where
    start = moveMaxCards' False n frDeck toDeck

    moveMaxCards' :: Bool -> Int -> Deck -> Deck -> Maybe (Deck, Deck)
    moveMaxCards' moved 0    fr to = CC.justIf moved (fr, to)
    moveMaxCards' moved pend fr to = CC.handleMaybe {-if-} (moveCard fr to)
                                                    {-then-} continue
                                                    {-else-} (moveMaxCards' moved 0 fr to)

      where continue (fr', to') = moveMaxCards' True (pred pend) fr' to'

