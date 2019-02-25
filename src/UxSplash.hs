{-# OPTIONS_GHC -Wall #-}
module UxSplash (splashScreen) where

import Data.List (intercalate)

import UxColors (Color(..), formatLines)

cls :: String
cls = "\ESC[2J"
      
splashScreen :: String
splashScreen = cls ++ (intercalate "\n" (formatLines  splashScreenTxt  splashScreenFmt))

splashScreenTxt :: [String]
splashScreenTxt = [  -- TODO 2 colors
    " ╔══════════════════════════════════════╗ "
  , " ║ │   │ ╓── ╓─╖ ║  ▪ ─╥─ ╓─╖ ▪ ╓─╖ ╓── ║ "
  , " ║ ╞═══╡ ╙─╖ ║ ║ ║  ║  ║  ║ ║ ║ ╟─╜ ╟─  ║ "
  , " ║ │   │ ──╜ ╙─╜ ╙─ ║  ║  ╟─╢ ║ ╟─╖ ╙── ║ "
  , " ╚══════════════════════════════════════╝ "
  , "  made with           Klondike Solitaire  "
  , "   λ Haskel         for (some) terminals  "
  , "                               A  ♥ ╔══╗  "
  , "  Developed by Crul                 ║§§║  "
  , "  github.com/Crul/HSolitaire   ♥  A ╚══╝  "
  , "                                          "
  , "        - Press any key to start -        "
  , "                                          "
  ]


bgrTitleFmt :: (Color,Color,Int)
bgrTitleFmt = (Blue,YellowL,38)

bgrFrameFmt :: Int -> (Color,Color,Int)
bgrFrameFmt i = (Blue,BlueL,i)

subTitleFmt :: (Color,Color,Int)
subTitleFmt = (Dfl,Green,31)

splashScreenFmt :: [[(Color,Color,Int)]]
splashScreenFmt = [
    [(Dfl,Dfl,1)                   ,bgrFrameFmt 40              ,(Dfl,Dfl,1)]
  , [(NC,NC,1)     ,bgrFrameFmt 1  ,bgrTitleFmt   ,bgrFrameFmt 1,(Dfl,Dfl,1)]
  , [(NC,NC,1)     ,bgrFrameFmt 1  ,bgrTitleFmt   ,bgrFrameFmt 1,(Dfl,Dfl,1)]
  , [(NC,NC,1)     ,bgrFrameFmt 1  ,bgrTitleFmt   ,bgrFrameFmt 1,(Dfl,Dfl,1)]
  , [(NC,NC,1)                     ,bgrFrameFmt 40              ,(Dfl,Dfl,1)]
  , [(NC,NC,2)     ,(Dfl,MagentaL,9),subTitleFmt]
  , [(NC,NC,3)     ,(Dfl,MagentaL,8),subTitleFmt]
  , [(NC,NC,31)    ,(White,Red,4)  ,(Dfl,Dfl,1),(Blue,BlueL,4),(Dfl,Dfl,3)]
  , [(NC,BlackL,31) ,(White,Red,4)  ,(Dfl,Dfl,1),(Blue,BlueL,4),(Dfl,Dfl,3)]
  , [(Dfl,Blue,31) ,(White,Red,4)  ,(Dfl,Dfl,1),(Blue,BlueL,4),(Dfl,Dfl,3)]
  , [(NC,NC,42)]
  , [(NC,NC, 7)    ,(Red,WhiteL,28),(Dfl,Dfl, 7)]
  , [(NC,NC,42)]
  ]
