{-# OPTIONS_GHC -Wall #-}
module Actions (action, loopToSuitDeck', colToSuitDeck') where

import qualified Data.Vector as V

import qualified Crosscutting as CC
import Decks      (setSuitDeck, suitDeck, nextCard, moveMaxCards)
import State      (State(..), addMessage, stColumnDeck, setStColumnDeck, setStColumnDecks, setLoop)
import ActionsFns (revealCard, colDeckToColDeck, cardFromDeckToSuitDeck)

takeCards :: Int
takeCards = 3

err :: State -> String -> State
err = addMessage

wrongCol :: Int -> Bool
wrongCol c = not $ c `elem` [1..7] -- TODO 3 DRY
    
wrongSuit :: Int -> Bool
wrongSuit s = not $ s `elem` [1..4]


action :: State -> String -> State
action stt act = case act of
    ""        -> takeLoop stt
    "."       -> loopToSuitDeck stt
--  ".."      -> loopKingToEmptyColDeck stt TODO 2
    [c]       -> CC.runIntParam c (colToSuitDeck stt) wrongMsg
    ['.',c]   -> CC.runIntParam c (loopToColumn stt) wrongMsg
    [a,b]     -> CC.runIntParam2 a b (colToCol stt) wrongMsg
    [s,'/',c] -> CC.runIntParam2 s c (suitDeckToColumn stt) wrongMsg -- TODO ? change 1/4 --> s/4
    _         -> wrongMsg

  where wrongMsg = err stt "Sorry, I don't get it"


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

    cardsMoved = cardFromDeckToSuitDeck (suitDecks stt) (nextCard vLD)

    setDecks (vLD',sDs') = stt { loopDecks = (hLD,vLD')
                               , suitDecks = sDs'
                               }


colToSuitDeck :: State -> Int -> State
colToSuitDeck stt col | wrongCol col = err stt "Wrong column, use from 1 to 7"  -- TODO 3 DRY
                      | otherwise    = CC.maybeOr (colToSuitDeck' stt col)
                                                  (err stt $ "No positionable card in column " ++ (show col))

colToSuitDeck' :: State -> Int -> Maybe State
colToSuitDeck' stt col = CC.chainMaybe {-if-} decksMoved {-then-} setDecks
  where
    (hCol,vCol) = stColumnDeck stt col

    decksMoved  = cardFromDeckToSuitDeck (suitDecks stt) (nextCard vCol)

    setDecks (vCol',stDs') = setStColumnDeck stt' col colD
                where stt' = stt { suitDecks = stDs' }
                      colD = revealCard (hCol,vCol')


loopToColumn :: State -> Int -> State
loopToColumn stt toC = CC.handleMaybe {-if-} decksMoved
                                      {-then-} setDecks
                                      {-else-} (err stt $ "No moves are possible from Main Deck to Column " ++ (show toC))
  where
    loopDs     = loopDecks stt
    toColDeck  = stColumnDeck stt toC

    decksMoved = colDeckToColDeck loopDs toColDeck

    setDecks (loopDs', toColDeck') = setStColumnDeck stt' toC toColDeck'
                        where stt' = stt { loopDecks=loopDs' }


colToCol :: State -> Int -> Int -> State
colToCol stt frC toC = if frC == toC
                         then err stt "Cannot move to the same column"  -- TODO 2 make XX to try to move to suit deck adn/or king to empty column deck
                         else CC.handleMaybe {-if-} decksMoved
                                             {-then-} setDecks
                                             {-else-} (err stt $ "Cannot move from column " ++ (show frC) ++ " to column " ++ (show toC))
  where
    frCD       = stColumnDeck stt frC
    toCD       = stColumnDeck stt toC

    decksMoved = colDeckToColDeck frCD toCD

    setDecks (frCD', toCD') = setStColumnDecks stt changes
              where changes = [(frC,frCD'),(toC,toCD')]


suitDeckToColumn :: State -> Int -> Int -> State
suitDeckToColumn stt st toC | wrongCol toC = err stt "Wrong column, use from 1 to 7"  -- TODO 3 DRY
                            | wrongSuit st = err stt "Wrong Suit Deck, use from 1 to 4"
                            | otherwise    = CC.handleMaybe {-if-} decksMoved
                                                            {-then-} setDecks
                                                            {-else-} (err stt $ "Cannot move positioned card to column " ++ (show toC))
  where
    fromCD     = (V.empty, suitDeck (suitDecks stt) st)
    toCD       = stColumnDeck stt toC

    decksMoved = colDeckToColDeck fromCD toCD

    setDecks ((_,suitD), toCD') = setStColumnDeck stt' toC toCD'
                     where stt' = stt { suitDecks = setSuitDeck (suitDecks stt) st suitD }

