module Opcode (opcode) where

import Data.Bits
import Data.Word

opcode :: Word32 -> (Int, Int)
opcode a = (opcd, xo)
  where

  field :: Int -> Int -> Int
  field h l = shiftR (fromIntegral a) (31 - l) .&. foldl setBit 0 [0 .. l - h]

  opcd  = field 0 5

  xoDS  = field 30 31
  xoX   = field 21 30
  xoXL  = xoX
  xoXFX = xoX
  xoXFL = xoX
  xoXS  = field 21 29
  xoXO  = field 22 30
  xoA   = field 26 30
  xoMD  = field 27 29
  xoMDS = field 27 30
  xoD   = 0
  xoSC  = 0
  xoI   = 0
  xoM   = 0
  xoB   = 0

  xo | opcd ==  2 && xoD    ==    0 = xoD
     | opcd ==  3 && xoD    ==    0 = xoD
     | opcd ==  7 && xoD    ==    0 = xoD
     | opcd ==  8 && xoD    ==    0 = xoD
     | opcd == 10 && xoD    ==    0 = xoD
     | opcd == 11 && xoD    ==    0 = xoD
     | opcd == 12 && xoD    ==    0 = xoD
     | opcd == 13 && xoD    ==    0 = xoD
     | opcd == 14 && xoD    ==    0 = xoD
     | opcd == 15 && xoD    ==    0 = xoD
     | opcd == 16 && xoB    ==    0 = xoB
     | opcd == 17 && xoSC   ==    0 = xoSC
     | opcd == 18 && xoI    ==    0 = xoI
     | opcd == 19 && xoXL   ==    0 = xoXL
     | opcd == 19 && xoXL   ==   16 = xoXL
     | opcd == 19 && xoXL   ==   18 = xoXL
     | opcd == 19 && xoXL   ==   33 = xoXL
     | opcd == 19 && xoXL   ==  129 = xoXL
     | opcd == 19 && xoXL   ==  150 = xoXL
     | opcd == 19 && xoXL   ==  193 = xoXL
     | opcd == 19 && xoXL   ==  225 = xoXL
     | opcd == 19 && xoXL   ==  257 = xoXL
     | opcd == 19 && xoXL   ==  274 = xoXL
     | opcd == 19 && xoXL   ==  289 = xoXL
     | opcd == 19 && xoXL   ==  417 = xoXL
     | opcd == 19 && xoXL   ==  449 = xoXL
     | opcd == 19 && xoXL   ==  528 = xoXL
     | opcd == 20 && xoM    ==    0 = xoM
     | opcd == 21 && xoM    ==    0 = xoM
     | opcd == 23 && xoM    ==    0 = xoM
     | opcd == 24 && xoD    ==    0 = xoD
     | opcd == 25 && xoD    ==    0 = xoD
     | opcd == 26 && xoD    ==    0 = xoD
     | opcd == 27 && xoD    ==    0 = xoD
     | opcd == 28 && xoD    ==    0 = xoD
     | opcd == 29 && xoD    ==    0 = xoD
     | opcd == 30 && xoMD   ==    0 = xoMD
     | opcd == 30 && xoMD   ==    1 = xoMD
     | opcd == 30 && xoMD   ==    2 = xoMD
     | opcd == 30 && xoMD   ==    3 = xoMD
     | opcd == 30 && xoMDS  ==    8 = xoMDS
     | opcd == 30 && xoMDS  ==    9 = xoMDS
     | opcd == 31 && xoX    ==    0 = xoX
     | opcd == 31 && xoX    ==    4 = xoX
     | opcd == 31 && xoXO   ==    8 = xoXO
     | opcd == 31 && xoXO   ==    9 = xoXO
     | opcd == 31 && xoXO   ==   10 = xoXO
     | opcd == 31 && xoXO   ==   11 = xoXO
     | opcd == 31 && xoXFX  ==   19 = xoXFX
     | opcd == 31 && xoXFX  ==   19 = xoXFX
     | opcd == 31 && xoX    ==   20 = xoX
     | opcd == 31 && xoX    ==   21 = xoX
     | opcd == 31 && xoX    ==   23 = xoX
     | opcd == 31 && xoX    ==   24 = xoX
     | opcd == 31 && xoX    ==   26 = xoX
     | opcd == 31 && xoX    ==   27 = xoX
     | opcd == 31 && xoX    ==   28 = xoX
     | opcd == 31 && xoX    ==   32 = xoX
     | opcd == 31 && xoXO   ==   40 = xoXO
     | opcd == 31 && xoX    ==   53 = xoX
     | opcd == 31 && xoX    ==   54 = xoX
     | opcd == 31 && xoX    ==   55 = xoX
     | opcd == 31 && xoX    ==   58 = xoX
     | opcd == 31 && xoX    ==   60 = xoX
     | opcd == 31 && xoX    ==   68 = xoX
     | opcd == 31 && xoXO   ==   73 = xoXO
     | opcd == 31 && xoXO   ==   75 = xoXO
     | opcd == 31 && xoX    ==   83 = xoX
     | opcd == 31 && xoX    ==   84 = xoX
     | opcd == 31 && xoX    ==   86 = xoX
     | opcd == 31 && xoX    ==   87 = xoX
     | opcd == 31 && xoXO   ==  104 = xoXO
     | opcd == 31 && xoX    ==  119 = xoX
     | opcd == 31 && xoX    ==  122 = xoX
     | opcd == 31 && xoX    ==  124 = xoX
     | opcd == 31 && xoXO   ==  136 = xoXO
     | opcd == 31 && xoXO   ==  138 = xoXO
     | opcd == 31 && xoXFX  ==  144 = xoXFX
     | opcd == 31 && xoXFX  ==  144 = xoXFX
     | opcd == 31 && xoX    ==  146 = xoX
     | opcd == 31 && xoX    ==  149 = xoX
     | opcd == 31 && xoX    ==  150 = xoX
     | opcd == 31 && xoX    ==  151 = xoX
     | opcd == 31 && xoX    ==  178 = xoX
     | opcd == 31 && xoX    ==  181 = xoX
     | opcd == 31 && xoX    ==  183 = xoX
     | opcd == 31 && xoXO   ==  200 = xoXO
     | opcd == 31 && xoXO   ==  202 = xoXO
     | opcd == 31 && xoX    ==  210 = xoX
     | opcd == 31 && xoX    ==  214 = xoX
     | opcd == 31 && xoX    ==  215 = xoX
     | opcd == 31 && xoXO   ==  232 = xoXO
     | opcd == 31 && xoXO   ==  233 = xoXO
     | opcd == 31 && xoXO   ==  234 = xoXO
     | opcd == 31 && xoXO   ==  235 = xoXO
     | opcd == 31 && xoX    ==  242 = xoX
     | opcd == 31 && xoX    ==  246 = xoX
     | opcd == 31 && xoX    ==  247 = xoX
     | opcd == 31 && xoXO   ==  266 = xoXO
     | opcd == 31 && xoX    ==  278 = xoX
     | opcd == 31 && xoX    ==  279 = xoX
     | opcd == 31 && xoX    ==  284 = xoX
     | opcd == 31 && xoX    ==  306 = xoX
     | opcd == 31 && xoX    ==  310 = xoX
     | opcd == 31 && xoX    ==  311 = xoX
     | opcd == 31 && xoX    ==  316 = xoX
     | opcd == 31 && xoXFX  ==  339 = xoXFX
     | opcd == 31 && xoX    ==  341 = xoX
     | opcd == 31 && xoX    ==  343 = xoX
     | opcd == 31 && xoX    ==  370 = xoX
     | opcd == 31 && xoXFX  ==  371 = xoXFX
     | opcd == 31 && xoX    ==  373 = xoX
     | opcd == 31 && xoX    ==  375 = xoX
     | opcd == 31 && xoX    ==  402 = xoX
     | opcd == 31 && xoX    ==  407 = xoX
     | opcd == 31 && xoX    ==  412 = xoX
     | opcd == 31 && xoXS   ==  413 = xoXS
     | opcd == 31 && xoX    ==  434 = xoX
     | opcd == 31 && xoX    ==  438 = xoX
     | opcd == 31 && xoX    ==  439 = xoX
     | opcd == 31 && xoX    ==  444 = xoX
     | opcd == 31 && xoXO   ==  457 = xoXO
     | opcd == 31 && xoXO   ==  459 = xoXO
     | opcd == 31 && xoXFX  ==  467 = xoXFX
     | opcd == 31 && xoX    ==  476 = xoX
     | opcd == 31 && xoXO   ==  489 = xoXO
     | opcd == 31 && xoXO   ==  491 = xoXO
     | opcd == 31 && xoX    ==  498 = xoX
     | opcd == 31 && xoX    ==  512 = xoX
     | opcd == 31 && xoX    ==  533 = xoX
     | opcd == 31 && xoX    ==  534 = xoX
     | opcd == 31 && xoX    ==  535 = xoX
     | opcd == 31 && xoX    ==  536 = xoX
     | opcd == 31 && xoX    ==  539 = xoX
     | opcd == 31 && xoX    ==  566 = xoX
     | opcd == 31 && xoX    ==  567 = xoX
     | opcd == 31 && xoX    ==  595 = xoX
     | opcd == 31 && xoX    ==  597 = xoX
     | opcd == 31 && xoX    ==  598 = xoX
     | opcd == 31 && xoX    ==  599 = xoX
     | opcd == 31 && xoX    ==  631 = xoX
     | opcd == 31 && xoX    ==  659 = xoX
     | opcd == 31 && xoX    ==  661 = xoX
     | opcd == 31 && xoX    ==  662 = xoX
     | opcd == 31 && xoX    ==  663 = xoX
     | opcd == 31 && xoX    ==  695 = xoX
     | opcd == 31 && xoX    ==  725 = xoX
     | opcd == 31 && xoX    ==  727 = xoX
     | opcd == 31 && xoX    ==  759 = xoX
     | opcd == 31 && xoX    ==  790 = xoX
     | opcd == 31 && xoX    ==  792 = xoX
     | opcd == 31 && xoX    ==  794 = xoX
     | opcd == 31 && xoX    ==  824 = xoX
     | opcd == 31 && xoX    ==  851 = xoX
     | opcd == 31 && xoX    ==  854 = xoX
     | opcd == 31 && xoX    ==  915 = xoX
     | opcd == 31 && xoX    ==  918 = xoX
     | opcd == 31 && xoX    ==  922 = xoX
     | opcd == 31 && xoX    ==  954 = xoX
     | opcd == 31 && xoX    ==  982 = xoX
     | opcd == 31 && xoX    ==  983 = xoX
     | opcd == 31 && xoX    ==  986 = xoX
     | opcd == 31 && xoX    == 1014 = xoX
     | opcd == 32 && xoD    ==    0 = xoD
     | opcd == 33 && xoD    ==    0 = xoD
     | opcd == 34 && xoD    ==    0 = xoD
     | opcd == 35 && xoD    ==    0 = xoD
     | opcd == 36 && xoD    ==    0 = xoD
     | opcd == 37 && xoD    ==    0 = xoD
     | opcd == 38 && xoD    ==    0 = xoD
     | opcd == 39 && xoD    ==    0 = xoD
     | opcd == 40 && xoD    ==    0 = xoD
     | opcd == 41 && xoD    ==    0 = xoD
     | opcd == 42 && xoD    ==    0 = xoD
     | opcd == 43 && xoD    ==    0 = xoD
     | opcd == 44 && xoD    ==    0 = xoD
     | opcd == 45 && xoD    ==    0 = xoD
     | opcd == 46 && xoD    ==    0 = xoD
     | opcd == 47 && xoD    ==    0 = xoD
     | opcd == 48 && xoD    ==    0 = xoD
     | opcd == 49 && xoD    ==    0 = xoD
     | opcd == 50 && xoD    ==    0 = xoD
     | opcd == 51 && xoD    ==    0 = xoD
     | opcd == 52 && xoD    ==    0 = xoD
     | opcd == 53 && xoD    ==    0 = xoD
     | opcd == 54 && xoD    ==    0 = xoD
     | opcd == 55 && xoD    ==    0 = xoD
     | opcd == 58 && xoDS   ==    0 = xoDS
     | opcd == 58 && xoDS   ==    1 = xoDS
     | opcd == 58 && xoDS   ==    2 = xoDS
     | opcd == 59 && xoA    ==   18 = xoA
     | opcd == 59 && xoA    ==   20 = xoA
     | opcd == 59 && xoA    ==   21 = xoA
     | opcd == 59 && xoA    ==   22 = xoA
     | opcd == 59 && xoA    ==   24 = xoA
     | opcd == 59 && xoA    ==   25 = xoA
     | opcd == 59 && xoA    ==   26 = xoA
     | opcd == 59 && xoA    ==   28 = xoA
     | opcd == 59 && xoA    ==   29 = xoA
     | opcd == 59 && xoA    ==   30 = xoA
     | opcd == 59 && xoA    ==   31 = xoA
     | opcd == 62 && xoDS   ==    0 = xoDS
     | opcd == 62 && xoDS   ==    1 = xoDS
     | opcd == 63 && xoX    ==    0 = xoX
     | opcd == 63 && xoX    ==   12 = xoX
     | opcd == 63 && xoX    ==   14 = xoX
     | opcd == 63 && xoX    ==   15 = xoX
     | opcd == 63 && xoA    ==   18 = xoA
     | opcd == 63 && xoA    ==   20 = xoA
     | opcd == 63 && xoA    ==   21 = xoA
     | opcd == 63 && xoA    ==   22 = xoA
     | opcd == 63 && xoA    ==   23 = xoA
     | opcd == 63 && xoA    ==   24 = xoA
     | opcd == 63 && xoA    ==   25 = xoA
     | opcd == 63 && xoA    ==   26 = xoA
     | opcd == 63 && xoA    ==   28 = xoA
     | opcd == 63 && xoA    ==   29 = xoA
     | opcd == 63 && xoA    ==   30 = xoA
     | opcd == 63 && xoA    ==   31 = xoA
     | opcd == 63 && xoX    ==   32 = xoX
     | opcd == 63 && xoX    ==   38 = xoX
     | opcd == 63 && xoX    ==   40 = xoX
     | opcd == 63 && xoX    ==   64 = xoX
     | opcd == 63 && xoX    ==   70 = xoX
     | opcd == 63 && xoX    ==   72 = xoX
     | opcd == 63 && xoX    ==  134 = xoX
     | opcd == 63 && xoX    ==  136 = xoX
     | opcd == 63 && xoX    ==  264 = xoX
     | opcd == 63 && xoX    ==  583 = xoX
     | opcd == 63 && xoXFL  ==  711 = xoXFL
     | opcd == 63 && xoX    ==  814 = xoX
     | opcd == 63 && xoX    ==  815 = xoX
     | opcd == 63 && xoX    ==  846 = xoX
     | otherwise                    = 0
