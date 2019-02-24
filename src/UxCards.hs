{-# OPTIONS_GHC -Wall #-}
module UxCards ( showCardRows
               , showEmptyCard
               , showBackCard
               , showFullCard
               , showLeftSideCard
               , showTopHalfBackCard
               , showTopHalfCard ) where

import Cards (Card(..), Label, suitColorCmd)

showCardRows :: [Int]
showCardRows = [0..2]

-- https://en.wikipedia.org/wiki/ANSI_escape_code
showEmptyCard :: Int -> String
showEmptyCard 0 = "\ESC[100;37m -- \ESC[0m "
showEmptyCard 1 = "\ESC[100;37m|  |\ESC[0m "
showEmptyCard 2 = "\ESC[100;37m -- \ESC[0m "
showEmptyCard _ = ""

-- https://unicode-table.com
showBackCard :: Int -> String
showBackCard 0 = "\ESC[44;94m╔══╗\ESC[0m "
showBackCard 1 = "\ESC[44;94m║§§║\ESC[0m "
showBackCard 2 = "\ESC[44;94m╚══╝\ESC[0m "
showBackCard _ = ""

showFullCard :: Card -> Int -> String
showFullCard (Card lb st) 0 = "\ESC[107" ++ suitColorCmd st ++ showLb SpRight lb ++ " " ++ show st ++ "\ESC[0m "
showFullCard _            1 = "\ESC[107;97m    \ESC[0m "
showFullCard (Card lb st) 2 = "\ESC[107" ++ suitColorCmd st ++ show st ++ " " ++ showLb SpLeft lb ++ "\ESC[0m "
-- showFullCard (Card lb st) 2 = "\ESC[107" ++ suitColorCmd st ++ " " ++ show st ++ showLb SpLeft lb ++ "\ESC[0m"
showFullCard _            _ = ""

showLeftSideCard :: Card -> Int -> String
showLeftSideCard (Card lb st) 0 = "\ESC[47" ++ suitColorCmd st ++ showLb SpRight lb ++ "\ESC[0m"
showLeftSideCard _            1 = "\ESC[47;37m  \ESC[0m"
showLeftSideCard (Card _  st) 2 = "\ESC[47" ++ suitColorCmd st ++ show st ++ " " ++ "\ESC[0m"
showLeftSideCard _            _ = ""

showTopHalfBackCard :: Card -> [String]
showTopHalfBackCard _ = [showBackCard 0]

showTopHalfCard :: Card -> [String]
showTopHalfCard (Card lb st) = ["\ESC[47" ++ suitColorCmd st ++ showLb SpRight lb ++ " " ++ show st ++ "\ESC[0m "]

data SpLeftOrRight = SpLeft | SpRight

showLb :: SpLeftOrRight -> Label -> String
showLb lor lb = addSpace $ show lb
  where addSpace str = if length str > 1
                       then str
                       else addSpace' lor str

        addSpace' SpLeft  str = " " ++ str
        addSpace' SpRight str = str ++ " "
