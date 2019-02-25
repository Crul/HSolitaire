{-# OPTIONS_GHC -Wall #-}
module ActionsFns (revealCard, deckToColDeck, cardToSuitDeck) where

import Control.Monad
import qualified Data.Vector as V

import qualified Crosscutting as CC
import Cards (Card(..), Label(..), suitColor, succLabel)
import Decks ( Deck
             , SuitDecks
             , ColumnDeck
             , NextCardResult
             , nextCard
             , moveCard
             , suitDeckIdx
             , suitDeck
             , setSuitDeck
             )
             
revealCard :: ColumnDeck -> ColumnDeck
revealCard (hid,vis) | V.null vis = CC.maybeOr (moveCard hid vis) {-or-} (hid, vis)
                     | otherwise  = (hid, vis)

----------------------------------------------------------------------------------------------------

deckToColDeck :: ColumnDeck -> ColumnDeck -> Maybe (ColumnDeck, ColumnDeck)
deckToColDeck frCD@(_,vFrD) toCD@(_,vToD) = colDecks'
  where
    colDecks  = CC.handleMaybe {-if-} (nextCard vToD)
                               {-then-} (deckOverDeckCard frCD toCD)
                               {-else-} (deckToEmpty frCD toCD)

    colDecks' = CC.continueMaybe {-if-} colDecks {-then-} reveal
  
    reveal (frCD', toCD') = CC.maybeIfVal (not $ V.null vFrD)
                                          (revealCard frCD', toCD')


deckToEmpty :: ColumnDeck -> ColumnDeck -> Maybe (ColumnDeck, ColumnDeck)
deckToEmpty (hFrD,vFrD) (hToD,_) = CC.justIf isTopCardKing movedDecks
  where isTopCardKing = isKing $ nextCard (V.reverse vFrD)
        movedDecks    = ((hFrD,V.empty), (hToD,vFrD))


isKing :: Maybe NextCardResult -> Bool
isKing (Just ((Card King _), _)) = True
isKing _                         = False


deckOverDeckCard :: ColumnDeck -> ColumnDeck -> NextCardResult -> Maybe (ColumnDeck, ColumnDeck)
deckOverDeckCard (hFrD,vFrD) (hToD,vToD) (overCard,_) = decks
  where
    moveMovable' = liftM moveMovable
    movablePart  = stackableSubDeck vFrD overCard 
    decks        = moveMovable' movablePart

    moveMovable (vFrD', movable) = (frCD, toCD)
      where frCD = (hFrD, vFrD')
            toCD = (hToD, movable V.++ vToD)


stackableSubDeck :: Deck -> Card -> Maybe (Deck, Deck)
stackableSubDeck = stackableSubDeck' V.empty

stackableSubDeck' :: Deck -> Deck -> Card -> Maybe (Deck, Deck)
stackableSubDeck' movable frDeck overCard = CC.maybeIf (not $ V.null frDeck) continue
  where
    Just (card, frDeck') = nextCard frDeck
    movable'             = movable V.++ (V.fromList [card])
    continue = if stackable card overCard
                  then Just (frDeck', movable')
                  else stackableSubDeck' movable' frDeck' overCard


stackable :: Card -> Card -> Bool
stackable (Card King _)            _                      = False
stackable (Card stCrdLbl stCrdSt) (Card ovCrdLbl ovCrdSt) =
  (succLabel stCrdLbl == ovCrdLbl) && (suitColor stCrdSt) /= (suitColor ovCrdSt)  -- TODO 3 sameColor stCrdSt ovCrdSt

----------------------------------------------------------------------------------------------------

cardToSuitDeck :: SuitDecks -> Maybe NextCardResult -> Maybe (Deck, SuitDecks)
cardToSuitDeck stDecks mbCrdRes = CC.continueMaybe {-if-} mbCrdRes
                                                   {-then-} (cardToSuitDeck' stDecks)

cardToSuitDeck' :: SuitDecks -> NextCardResult -> Maybe (Deck, SuitDecks)
cardToSuitDeck' sDs crdRes@(crd,_) = cardToSuitDeck'' crdRes suitDec (setSuitDeck sDs sDIdx)
  where sDIdx   = suitDeckIdx crd 
        suitDec = suitDeck sDs sDIdx


cardToSuitDeck'' :: NextCardResult -> Deck -> (Deck -> SuitDecks) -> Maybe (Deck, SuitDecks)
cardToSuitDeck'' (card, frDck) stDck getSuitDecks = movedDecks
  where 
    movedDecks = CC.chainMaybe {-if-} (cardToSuitDeck''' card stDck)
                               {-then-} (\stDck' -> (frDck, getSuitDecks stDck'))

cardToSuitDeck''' :: Card -> Deck -> Maybe Deck
cardToSuitDeck''' crd@(Card lb _) deck = case nextCard deck of
    Nothing                 -> setAce
    Just ((Card King _), _) -> Nothing
    Just ((Card lb'  _), _) -> setIf $ succLabel lb'
  where
    setAce    = CC.justIf (lb == Ace) (V.fromList [crd])
    setIf lb' = CC.justIf (lb == lb') (V.cons crd deck)
