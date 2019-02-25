{-# OPTIONS_GHC -Wall #-}
module UxDecks ( showLoopDecks
               , showSuitDecks
               , showColumnDecks
               ) where

import qualified Data.Vector as V

import qualified Crosscutting as CC
import Decks   ( Deck
               , LoopDecks
               , SuitDecks
               , ColumnDeck
               , ColumnDecks
               , nextCard
               )

import UxCards ( showCardRows
               , showEmptyCard
               , showBackCard
               , showFullCard
               , showLeftSideCard
               , showTopHalfBackCard
               , showTopHalfCard
               )

colDeckWitdth :: Int
colDeckWitdth = 5

emptyRows :: [String]
emptyRows = take (length showCardRows) $ repeat ""

showLoopDecks :: LoopDecks -> [String]
showLoopDecks (hLD,vLD) = ["Main Deck     "] ++ CC.concatLines [showBackDeck hLD, showVisLoop vLD]

showSuitDecks :: SuitDecks -> [String]
showSuitDecks (sD, hD, cD, dD) = sep ++ titlesTop ++ top ++ sep ++ bottom ++ titlesBottom
  where
    titlesTop     = [" F0   F1      "]
    top           = showSDPair [sD,hD]
    sep           = [take 14 $ repeat ' ']
    bottom        = showSDPair [cD,dD]
    titlesBottom  = [" F2   F3      "]
    showSDPair ds = (map (\s -> s ++ "    ") $ CC.concatLines $ map showVisTopCard ds)

showColumnDecks :: ColumnDecks -> [String]
showColumnDecks (cD0,cD1,cD2,cD3,cD4,cD5,cD6) = shownColDs''
  where
    titles       = concat $ map (\i -> " C" ++ (show i) ++ "  ") [(0::Int)..6]
    shownColDs   = map showColumnDeck [cD0,cD1,cD2,cD3,cD4,cD5,cD6]
    shownColDs'' = [titles] ++ (CC.columnLayout $ map (\c -> (colDeckWitdth, c)) shownColDs)

-----------------------------------------------------------------------------------------------

showVisLoop :: Deck -> [String]
showVisLoop vLD = CC.concatLines [thirdCard, secndCard, firstCard, margin]
  where
    nCards    = length vLD
    firstCard = showVisTopCard vLD
    secndCard = if nCards < 2 then emptyRows else map (showLeftSideCard $ ((V.!) vLD 1)) showCardRows
    thirdCard = if nCards < 3 then emptyRows else map (showLeftSideCard $ ((V.!) vLD 2)) showCardRows
    marginCnt = min 4 (2 * (3 - nCards))
    margin    = take (length showCardRows) $ repeat (take marginCnt $ repeat ' ')


showColumnDeck :: ColumnDeck -> [String]
showColumnDeck (hidDeck, visDeck) = shownDeck'
  where shownDeck  = hidCards ++ visCards
        shownDeck' = if null shownDeck then map showEmptyCard showCardRows else shownDeck
        hidCards   = concat $ V.map showTopHalfBackCard hidDeck
        visCards   = if V.null visDeck
                        then []
                        else let Just (_, visDeck')  = nextCard visDeck
                                 halfCards           = concat $ V.map showTopHalfCard $ V.reverse visDeck'
                                 fullLastCard        = showVisTopCard visDeck
                             in halfCards ++ fullLastCard


showBackDeck :: Deck -> [String]
showBackDeck deck = map (showBackDeckRow deck) showCardRows

showBackDeckRow :: Deck -> Int -> String
showBackDeckRow deck = if V.null deck then showEmptyCard else showBackCard

showVisTopCard :: Deck -> [String]
showVisTopCard deck = map (showVisDeckRow deck) showCardRows

showVisDeckRow :: Deck -> Int -> String
showVisDeckRow deck = if V.null deck then showEmptyCard else showFullCard $ ((V.!) deck 0)
