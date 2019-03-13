{-# OPTIONS_GHC -Wall #-}
module UxCards ( showCardRows
               , showEmptyCard
               , showBackCard
               , showFullCard
               , showLeftSideCard
               , showTopHalfBackCard
               , showTopHalfCard ) where

import Data.Text (Text, pack)

import Cards (Card(..), Label, suitColorCmd)

showCardRows :: [Int]
showCardRows = [0..2]

-- https://en.wikipedia.org/wiki/ANSI_escape_code
showEmptyCard :: Int -> Text
showEmptyCard 0 = pack "\ESC[100;37m -- \ESC[0m "
showEmptyCard 1 = pack "\ESC[100;37m|  |\ESC[0m "
showEmptyCard 2 = pack "\ESC[100;37m -- \ESC[0m "
showEmptyCard _ = pack ""

-- https://unicode-table.com
showBackCard :: Int -> Text
showBackCard 0 = pack "\ESC[44;94m╔══╗\ESC[0m "
showBackCard 1 = pack "\ESC[44;94m║§§║\ESC[0m "
showBackCard 2 = pack "\ESC[44;94m╚══╝\ESC[0m "
showBackCard _ = pack ""

showFullCard :: Card -> Int -> Text
showFullCard (Card lb st) 0 = pack $ "\ESC[107" ++ suitColorCmd st ++ showLb SpRight lb ++ " " ++ show st ++ "\ESC[0m "
showFullCard _            1 = pack $ "\ESC[107;97m    \ESC[0m "
showFullCard (Card lb st) 2 = pack $ "\ESC[107" ++ suitColorCmd st ++ show st ++ " " ++ showLb SpLeft lb ++ "\ESC[0m "
-- showFullCard (Card lb st) 2 = pack $"\ESC[107" ++ suitColorCmd st ++ " " ++ show st ++ showLb SpLeft lb ++ "\ESC[0m"
showFullCard _            _ = pack $ ""

showLeftSideCard :: Card -> Int -> Text
showLeftSideCard (Card lb st) 0 = pack $ "\ESC[47" ++ suitColorCmd st ++ showLb SpRight lb ++ "\ESC[0m"
showLeftSideCard _            1 = pack $ "\ESC[47;37m  \ESC[0m"
showLeftSideCard (Card _  st) 2 = pack $ "\ESC[47" ++ suitColorCmd st ++ show st ++ " " ++ "\ESC[0m"
showLeftSideCard _            _ = pack $ ""

showTopHalfBackCard :: Card -> [Text]
showTopHalfBackCard _ = [showBackCard 0]

showTopHalfCard :: Card -> [Text]
showTopHalfCard (Card lb st) = [pack $ "\ESC[47" ++ suitColorCmd st ++ showLb SpRight lb ++ " " ++ show st ++ "\ESC[0m "]

data SpLeftOrRight = SpLeft | SpRight

showLb :: SpLeftOrRight -> Label -> Text
showLb lor lb = addSpace $ pack $ show lb
  where addSpace str = if length str > 1
                       then str
                       else addSpace' lor str

        addSpace' SpLeft  str = " " ++ str
        addSpace' SpRight str = str ++ " "
