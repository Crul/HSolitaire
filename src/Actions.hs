{-# OPTIONS_GHC -Wall #-}
module Actions (action, loopToSuitDeck', colToSuitDeck') where

import Data.Text (pack)
import qualified Data.Vector as V

import qualified Crosscutting as CC
import Decks      (setSuitDeck, suitDeck, nextCard, moveMaxCards)
import State      (State(..), addMessage, stColumnDeck, setStColumnDeck, setStColumnDecks, setLoop)
import ActionsFns (revealCard, deckToColDeck, cardToSuitDeck)

takeCards :: Int
takeCards = 3

err :: State -> String -> State
err stt msg = addMessage stt (pack msg)

wrongCol :: Int -> Bool
wrongCol c = not $ c `elem` [0..6] -- TODO 3 DRY
    
wrongSuit :: Int -> Bool
wrongSuit s = not $ s `elem` [0..3]


action :: State -> String -> State
action stt act = case act of
    ""        -> takeLoop stt
    "."       -> loopToSuitDeck stt
--  ".."      -> loopKingToEmptyColDeck stt TODO 4
    [c]       -> CC.runIntParam c (colToSuitDeck stt) wrongMsg
    ['.',c]   -> CC.runIntParam c (loopToColumn stt) wrongMsg
    [a,b]     -> CC.runIntParam2 a b (colToCol stt) wrongMsg
    [s,'/',c] -> CC.runIntParam2 s c (suitDeckToColumn stt) wrongMsg -- TODO 4 add [s|h|c|d]/[0..6]
    _         -> wrongMsg

  where wrongMsg = err stt "Sorry, I don't get it, type h for help"


takeLoop :: State -> State
takeLoop stt = CC.handleMaybe {-if-} cardsTaken
                              {-then-} setLoop'
                              {-else-} cycleOnEnd
  where
    loopDs     = loopDecks stt
    (_, vLD)   = loopDs
    cardsTaken = moveMaxCards takeCards loopDs

    setLoop' (h,v) = setLoop stt h v

    cycleOnEnd     = takeLoop $ setLoop' resetLoopDecks
    resetLoopDecks = (V.reverse vLD, V.empty)


loopToSuitDeck :: State -> State
loopToSuitDeck stt = CC.maybeOr (loopToSuitDeck' stt) (err stt "No positionable card in deck")

loopToSuitDeck' :: State -> Maybe State
loopToSuitDeck' stt = CC.chainMaybe {-if-} cardsMoved {-then-} setDecks
  where
    (hLD, vLD) = loopDecks stt

    cardsMoved = cardToSuitDeck (suitDecks stt) (nextCard vLD)

    setDecks (vLD',sDs') = stt { loopDecks = (hLD,vLD')
                               , suitDecks = sDs'
                               }


colToSuitDeck :: State -> Int -> State
colToSuitDeck stt col | wrongCol col = err stt "Wrong column, use from 0 to 6"  -- TODO 3 DRY
                      | otherwise    = CC.maybeOr (colToSuitDeck' stt col)
                                                  (err stt $ "No positionable card in column " ++ (show col))

colToSuitDeck' :: State -> Int -> Maybe State
colToSuitDeck' stt col = CC.chainMaybe {-if-} decksMoved {-then-} setDecks
  where
    (hCol,vCol) = stColumnDeck stt col

    decksMoved  = cardToSuitDeck (suitDecks stt) (nextCard vCol)

    setDecks (vCol',stDs') = setStColumnDeck stt' col colD
                where stt' = stt { suitDecks = stDs' }
                      colD = revealCard (hCol,vCol')


loopToColumn :: State -> Int -> State
loopToColumn stt toC = CC.handleMaybe {-if-} decksMoved
                                      {-then-} setDecks
                                      {-else-} (err stt $ "No moves are possible from Main Deck to Column " ++ (show toC))
  where
    (hLD, vLD)  = loopDecks stt
    fakeColDeck = (V.empty, V.take 1 vLD)
    toColDeck   = stColumnDeck stt toC

    decksMoved  = deckToColDeck fakeColDeck toColDeck

    setDecks (_, toColDeck') = setStColumnDeck stt' toC toColDeck'
                  where stt' = stt { loopDecks=(hLD, V.drop 1 vLD) }


colToCol :: State -> Int -> Int -> State
colToCol stt frC toC = if frC == toC
                         then err stt "Cannot move to the same column"  -- TODO 4 make XX to try to move to suit deck adn/or king to empty column deck
                         else CC.handleMaybe {-if-} decksMoved
                                             {-then-} setDecks
                                             {-else-} (err stt $ "Cannot move from column " ++ (show frC) ++ " to column " ++ (show toC))
  where
    frCD       = stColumnDeck stt frC
    toCD       = stColumnDeck stt toC

    decksMoved = deckToColDeck frCD toCD

    setDecks (frCD', toCD') = setStColumnDecks stt changes
              where changes = [(frC,frCD'),(toC,toCD')]


suitDeckToColumn :: State -> Int -> Int -> State
suitDeckToColumn stt st toC | wrongCol toC = err stt "Wrong column, use from 0 to 6"  -- TODO 3 DRY
                            | wrongSuit st = err stt "Wrong Suit Deck, use from 0 to 3"
                            | otherwise    = CC.handleMaybe {-if-} decksMoved
                                                            {-then-} setDecks
                                                            {-else-} (err stt $ "Cannot move positioned card to column " ++ (show toC))
  where
    fromCD     = (V.empty, suitDeck (suitDecks stt) st)
    toCD       = stColumnDeck stt toC

    decksMoved = deckToColDeck fromCD toCD

    setDecks ((_,suitD), toCD') = setStColumnDeck stt' toC toCD'
                     where stt' = stt { suitDecks = setSuitDeck (suitDecks stt) st suitD }

