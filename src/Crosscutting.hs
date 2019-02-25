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
                           (Just x)  -> fn x
                           _         -> err

runIntParam2 :: Char -> Char -> (Int -> Int -> a) -> a -> a
runIntParam2 a b fn err = case (mbInt a, mbInt b) of
                             (Just x, Just y) -> fn x y
                             _                -> err

mbInt :: Char -> Maybe Int
mbInt = readMaybe . (:[])

-- TODO 1 Data.Maybe.isJust :: Maybe a -> Bool
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

-- TODO 1 Data.Maybe.maybe :: b -> (a -> b) -> Maybe a -> b
handleMaybe :: Maybe a -> (a -> b) -> b -> b
handleMaybe Nothing  _  err = err
handleMaybe (Just a) fn _   = fn a

-- TODO 1 fmap :: Functor f => (a -> b) -> f a -> f b
chainMaybe :: Maybe a -> (a -> b) -> Maybe b
chainMaybe Nothing  _  = Nothing
chainMaybe (Just a) fn = Just $ fn a

-- TODO 1 (>>=) :: Monad m => m a -> (a -> m b) -> m b
continueMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
continueMaybe Nothing  _  = Nothing
continueMaybe (Just a) fn = fn a

justIf :: Bool -> a -> Maybe a
justIf True  a = Just a
justIf False _ = Nothing

maybeIf :: Bool -> Maybe a -> Maybe a
maybeIf True  a = a
maybeIf False _ = Nothing

-- TODO 1 Data.Maybe.fromMaybe :: a -> Maybe a -> a
maybeOr :: Maybe a -> a -> a
maybeOr (Just a)  _ = a
maybeOr Nothing   b = b

-- TODO 1 ??? maybeOrMaybe is <> if you wrap your Maybe into Data.Monoid.First
-- TODO 1 Control.Applicative (<|>) :: Alternative f => f a -> f a -> f a
maybeOrMaybe :: Maybe a -> Maybe a -> Maybe a
maybeOrMaybe Nothing mb = mb
maybeOrMaybe ma      _  = ma

maybeIfVal :: Bool -> a -> Maybe a
maybeIfVal cond a = maybeIf cond (Just a)
