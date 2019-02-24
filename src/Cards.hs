{-# OPTIONS_GHC -Wall #-}
module Cards ( Card(..)
             , Suit(..)
             , Label(..)
             , suits
             , labels
             , succLabel
             , suitColor
             , suitColorCmd) where

import Data.Char (chr)
import Data.Vector (Vector)
import qualified Data.Vector as V


data Card  = Card Label Suit

instance Show Card where  -- TODO 2 move to UX
    show (Card lb st) = "\ESC[107"
                     ++ (suitColorCmd st)
                     ++ (trim $ " " ++ show lb ++ show st ++ " ")
                     ++ "\ESC[0m"
      where trim = take 3 . reverse . take 4 . reverse

data Suit  = Spade | Heart | Club | Diamond
             deriving (Eq)

suitColor :: Suit -> Int  -- TODO ? export sameColor :: Suit -> Suit -> Boolean
suitColor Spade   = 0
suitColor Heart   = 1
suitColor Club    = 0
suitColor Diamond = 1

-- TODO 2 move to UX
suitColorCmd :: Suit -> String
suitColorCmd st | suitColor st == 0 = ";30m"
                | otherwise         = ";31m"

instance Show Suit where
    -- show Spade   = "S "
    -- show Heart   = "H "
    -- show Club    = "C "
    -- show Diamond = "D "
    show Spade   = [chr 6]
    show Heart   = [chr 3]
    show Club    = [chr 5]
    show Diamond = [chr 4]

data Label = Two   | Three | Four  | Five  | Six   | Seven
           | Eight | Nine  | Ten   | Jack  | Queen | King  | Ace
             deriving (Eq, Ord, Enum)

succLabel :: Label -> Label  -- TODO 2 deriving Ord/... ?
succLabel Ace   = Two
succLabel Two   = Three
succLabel Three = Four
succLabel Four  = Five
succLabel Five  = Six
succLabel Six   = Seven
succLabel Seven = Eight
succLabel Eight = Nine
succLabel Nine  = Ten
succLabel Ten   = Jack
succLabel Jack  = Queen
succLabel Queen = King
succLabel King  = Ace

instance Show Label where  -- TODO 2 move to UX
    show Jack  = "J"
    show Queen = "Q"
    show King  = "K"
    show Ace   = "A"
    show lbl   = show $ fromEnum lbl + 2

suits :: Vector Suit
suits = V.fromList [Spade, Heart, Club, Diamond]

labels :: Vector Label
labels = V.fromList [Two .. Ace]
