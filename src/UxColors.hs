{-# OPTIONS_GHC -Wall #-}
module UxColors (Color(..), formatLines) where

data Color = NC | Dfl | Black  | Red  | Green  | Yellow  | Blue  | Magenta  | Cyan  | White 
                      | BlackL | RedL | GreenL | YellowL | BlueL | MagentaL | CyanL | WhiteL

formatLines :: [String] -> [[(Color,Color,Int)]] -> [String]
formatLines = zipWith (formatLine [])

formatLine :: String -> String -> [(Color,Color,Int)] -> String
formatLine acc pend []         = acc ++ pend
formatLine acc pend (fmt:fmts) = formatLine acc' pend' fmts
  where
    (_,_,len) = fmt
    acc'      = acc ++ (applyFormat pend fmt)
    pend'     = drop len pend


applyFormat :: String -> (Color,Color,Int) -> String
applyFormat ln (bgC,txC,len) = fmt' ++ (take len ln)
  where fmt  = (bgrFormat bgC) ++ (txColor txC)
        fmt' = if null fmt then "" else ("\ESC[" ++ fmt)


bgrFormat :: Color -> String
bgrFormat NC  = ""
bgrFormat Dfl = bgrFormat Black
bgrFormat c   = fmt ++ ";"
  where fmt = case c of
              Black    -> "40"
              Red      -> "41"
              Green    -> "42"
              Yellow   -> "43"
              Blue     -> "44"
              Magenta  -> "45"
              Cyan     -> "46"
              White    -> "47"
              BlackL   -> "100"
              RedL     -> "101"
              GreenL   -> "102"
              YellowL  -> "103"
              BlueL    -> "104"
              MagentaL -> "105"
              CyanL    -> "106"
              WhiteL   -> "107"
              _        -> ""


txColor :: Color -> String
txColor NC  = ""
txColor Dfl = txColor White
txColor c   = fmt ++ "m"
  where fmt = case c of
              Black    -> "30"
              Red      -> "31"
              Green    -> "32"
              Yellow   -> "33"
              Blue     -> "34"
              Magenta  -> "35"
              Cyan     -> "36"
              White    -> "37"
              BlackL   -> "90"
              RedL     -> "91"
              GreenL   -> "92"
              YellowL  -> "93"
              BlueL    -> "94"
              MagentaL -> "95"
              CyanL    -> "96"
              WhiteL   -> "97"
              _        -> ""

