{-# OPTIONS_GHC -Wall #-}
module UxDecks ( showLoopDecks
               , showSuitDecks
               , showColumnDecks
               ) where

import Data.Text (Text)
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

emptyRows :: [Text]
emptyRows = take (length showCardRows) $ repeat $ pack ""

showLoopDecks :: LoopDecks -> [Text]
showLoopDecks (hLD,vLD) = [pack "Main Deck     "] ++ CC.concatLines [showBackDeck hLD, showVisLoop vLD]

showSuitDecks :: SuitDecks -> [Text]
showSuitDecks (sD, hD, cD, dD) = sep ++ titlesTop ++ top ++ sep ++ bottom ++ titlesBottom
  where
    titlesTop     = [pack " F0   F1      "]
    top           = showSDPair [sD,hD]
    sep           = [pack $ take 14 $ repeat ' ']
    bottom        = showSDPair [cD,dD]
    titlesBottom  = [pack " F2   F3      "]
    showSDPair ds = (map (\s -> s ++ pack "    ") $ CC.concatLines $ map showVisTopCard ds)

showColumnDecks :: ColumnDecks -> [Text]
showColumnDecks (cD0,cD1,cD2,cD3,cD4,cD5,cD6) = shownColDs''
  where
    titles       = pack $ concat $ map (\i -> " C" ++ (show i) ++ "  ") [(0::Int)..6]
    shownColDs   = map showColumnDeck [cD0,cD1,cD2,cD3,cD4,cD5,cD6]
    shownColDs'' = [titles] ++ (CC.columnLayout $ map (\c -> (colDeckWitdth, c)) shownColDs)

-----------------------------------------------------------------------------------------------

showVisLoop :: Deck -> [Text]
showVisLoop vLD = CC.concatLines [thirdCard, secndCard, firstCard, margin]
  where
    nCards    = length vLD
    firstCard = showVisTopCard vLD
    secndCard = if nCards < 2 then emptyRows else map (showLeftSideCard $ ((V.!) vLD 1)) showCardRows
    thirdCard = if nCards < 3 then emptyRows else map (showLeftSideCard $ ((V.!) vLD 2)) showCardRows
    marginCnt = min 4 (2 * (3 - nCards))
    margin    = take (length showCardRows) $ repeat (take marginCnt $ repeat ' ')


showColumnDeck :: ColumnDeck -> [Text]
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


showBackDeck :: Deck -> [Text]
showBackDeck deck = map (showBackDeckRow deck) showCardRows

showBackDeckRow :: Deck -> Int -> Text
showBackDeckRow deck = if V.null deck then showEmptyCard else showBackCard

showVisTopCard :: Deck -> [Text]
showVisTopCard deck = map (showVisDeckRow deck) showCardRows

showVisDeckRow :: Deck -> Int -> Text
showVisDeckRow deck = if V.null deck then showEmptyCard else showFullCard $ ((V.!) deck 0)
