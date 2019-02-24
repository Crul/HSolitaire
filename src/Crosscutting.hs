{-# OPTIONS_GHC -Wall #-}
module Crosscutting where

import Text.Read

columnLayout :: [(Int, [String])]  -> [String]
columnLayout sections = layout
  where
    maxHeight   = maximum $ map (\(_,lns) -> length lns) sections
    eqHeightScs = map (fillBlanks maxHeight) sections
    layout      = concatLines eqHeightScs

fillBlanks :: Int -> (Int, [String]) -> [String]
fillBlanks height (w, lns) = lns ++ blanks
  where
    blanks = take (height - (length lns)) $ repeat blank
    blank  = take w $ repeat ' '

concatLines :: [[String]]  -> [String]
concatLines lnArr = foldr concatLines' (repeat "") lnArr

concatLines' :: [String] -> [String] -> [String]
concatLines' xs ys = zipWith (++) xs ys

runIntParam :: Char -> (Int -> a) -> a -> a
runIntParam a fn err = case mbInt a of
                           (Just x)  -> fn (succ x)  -- TODO 1 succ
                           _        -> err

runIntParam2 :: Char -> Char -> (Int -> Int -> a) -> a -> a
runIntParam2 a b fn err = case (mbInt a, mbInt b) of
                             (Just x, Just y) -> fn (succ x) (succ y) -- TODO 2 succ
                             _                -> err

mbInt :: Char -> Maybe Int
mbInt = readMaybe . (:[])

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

handleMaybe :: Maybe a -> (a -> b) -> b -> b
handleMaybe Nothing  _  err = err
handleMaybe (Just a) fn _   = fn a

chainMaybe :: Maybe a -> (a -> b) -> Maybe b
chainMaybe Nothing  _  = Nothing
chainMaybe (Just a) fn = Just $ fn a

continueMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
continueMaybe Nothing  _  = Nothing
continueMaybe (Just a) fn = fn a

justIf :: Bool -> a -> Maybe a
justIf True  a = Just a
justIf False _ = Nothing

maybeIf :: Bool -> Maybe a -> Maybe a
maybeIf True  a = a
maybeIf False _ = Nothing

maybeOr :: Maybe a -> a -> a
maybeOr (Just a)  _ = a
maybeOr Nothing   b = b

maybeOrMaybe :: Maybe a -> Maybe a -> Maybe a
maybeOrMaybe Nothing mb = mb
maybeOrMaybe ma      _  = ma

maybeIfVal :: Bool -> a -> Maybe a
maybeIfVal cond a = maybeIf cond (Just a)
