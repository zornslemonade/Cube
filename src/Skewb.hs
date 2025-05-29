{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Skewb where

import qualified Algebra.Additive as Additive
import qualified Algebra.IntegralDomain as IntegralDomain
import qualified Algebra.Ring as Ring
import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.ZeroTestable as ZeroTestable
import Data.Group hiding (invert)
import qualified Data.List as L
import Data.Monoid
import Data.Semigroup
import Modular (Mod3, Mod4, Modular (unmod))
import NumericPrelude
import Action
import Permutation hiding (i)
import qualified Permutation as P
import Tuple
import TwistyPuzzle hiding (i)
import qualified Data.Function as F
import Control.Applicative
import qualified Data.Map as M
import qualified Data.IntMap as Map
import GHC.RTS.Flags (GCFlags(oldGenFactor))

type CenterP = Permutation Integer

type VertexP = Permutation Integer

type CenterO = Tuple6 Mod4

type VertexO = Tuple8 Mod3

newtype SkewbConfiguration = Skewb (CenterP, VertexP, CenterO, VertexO)

showSkewbConfig :: SkewbConfiguration -> String
showSkewbConfig (Skewb (a, b, xs, ys)) = L.intercalate "\n" [showInline a, showInline b, show xs, show ys]

instance Show SkewbConfiguration where
  show :: SkewbConfiguration -> String
  show = showSkewbConfig

-- | Type for representing individual pieces
data Skewbie = C Integer Mod4 | V Integer Mod3 deriving (Eq, Ord, Show)

isCenterSkewbie :: Skewbie -> Bool
isCenterSkewbie (C _ _) = True
isCenterSkewbie _ = False

isVertexSkewbie :: Skewbie -> Bool
isVertexSkewbie (V _ _) = True
isVertexSkewbie _ = False

getSkewbieNumber :: Skewbie -> Integer
getSkewbieNumber (C n _) = n
getSkewbieNumber (V n _) = n

------
-- Instantiating typeclasses
------

instance TwistyPuzzle SkewbConfiguration (CenterP, VertexP) (CenterO, VertexO) Skewbie where
  getPositions :: SkewbConfiguration -> (CenterP, VertexP)
  getPositions (Skewb (a, b, xs, ys)) = (a, b)

  getOrientations :: SkewbConfiguration -> (CenterO, VertexO)
  getOrientations (Skewb (a, b, xs, ys)) = (xs, ys)

  constructConfig :: (CenterP, VertexP) -> (CenterO, VertexO) -> SkewbConfiguration
  constructConfig (a, b) (xs, ys) = Skewb (a, b, xs, ys)

  getPieceData :: SkewbConfiguration -> [(Integer -> Integer -> Skewbie, Integer, Integer)]
  getPieceData _ = [(c, 6, 4), (v, 8, 3)]
    where
      c m = C m . fromInteger
      v m = V m . fromInteger

  -- \|
  -- Configurations of the skewb can also be seen as permutations of the set of stickers (where the 4 orientations of each center
  -- cubie sticker are considered distinct).
  -- This manifests as a monomorphism from the group of skewb configurations into the permutation group of stickers.
  -- Conversely, not every permutation of stickers gives a valid configuration of the skewb, for example a vertex sticker can never
  -- end up in the place of an edge sticker.
  --
  -- This sends a configuration to a permutation of the stickers, where each sticker is represented as a tuple (X, n, m), where
  -- X encodes whether it is a center, edge, or vertex cubie (taking the values 'C', 'E', or 'V', respectively), n represents the cubie
  -- the sticker is attached to, and m represents the face of that cubie that the sticker is attached to.
  -- For center cubies, m represents the orientation of the sticker.
  permutePiece :: SkewbConfiguration -> Skewbie -> Skewbie
  permutePiece (Skewb (a, b, xs, ys)) = \case
    C n k -> C (a ?. n) (xs *!! n + k)
    V n k -> V (b ?. n) (ys *!! n + k)
    where
      t *!! n = index t 0 n

  fromPermutation :: Permutation Skewbie -> Maybe SkewbConfiguration
  fromPermutation o = if all staysSame (fst <$> toPairs o) then Just $ Skewb (a, b, xs, ys) else Nothing
    where
      staysSame skewbie = case skewbie of
        C _ _ -> isCenterSkewbie (o ?. skewbie)
        V _ _ -> isVertexSkewbie (o ?. skewbie)
      getSkewbieNumbers = uncurry ((,) `F.on` getSkewbieNumber)
      a = pp $ map getSkewbieNumbers $ filter (isCenterSkewbie . fst) $ toPairs o
      b = pp $ map getSkewbieNumbers $ filter (isVertexSkewbie . fst) $ toPairs o
      xs = t6FromList [case o ?. C n 0 of C _ m -> m; _ -> 0 | n <- [1 .. 6]]
      ys = t8FromList [case o ?. V n 0 of V _ m -> m; _ -> 0 | n <- [1 .. 8]]

------
-- Explicitly writing out the Skewb configurations corresponding to the identity plus the single Turn of each face
------

-- Identity (no change)
i :: SkewbConfiguration
i = Skewb (P.i, P.i, 0, 0)

-- Corner 1: Upper Front Left (Clockwise)
ufl :: SkewbConfiguration
ufl = Skewb (p [[1, 2, 3]], p [[2, 4, 5]], t6 1 1 2 0 0 0, t8 1 2 0 2 2 0 0 0)

-- Corner 2: Upper Back Left (Clockwise)
ubl :: SkewbConfiguration
ubl = Skewb (p [[1, 3, 4]], p [[1, 8, 3]], t6 0 0 1 3 0 0, t8 2 1 2 0 0 0 0 2)

-- Corner 3: Upper Back Right (Clockwise)
ubr :: SkewbConfiguration
ubr = Skewb (p [[1, 4, 5]], p [[2, 7, 4]], t6 3 0 0 1 0 0, t8 0 2 1 2 0 0 2 0)

-- Corner 4: Upper Front Right (Clockwise)
ufr :: SkewbConfiguration
ufr = Skewb (p [[1, 5, 2]], p [[1, 3, 6]], t6 2 1 0 0 1 0, t8 2 0 2 1 0 2 0 0)

-- Corner 5: Lower Front Left (Clockwise)
dfl :: SkewbConfiguration
dfl = Skewb (p [[2, 6, 3]], p [[1, 6, 8]], t6 0 1 1 0 0 2, t8 2 0 0 0 1 0 2 2)

-- Corner 6: Lower Front Right (Clockwise)
dfr :: SkewbConfiguration
dfr = Skewb (p [[2, 5, 6]], p [[4, 7, 5]], t6 0 1 0 0 1 2, t8 0 0 0 2 2 1 2 0)

-- Corner 7: Lower Back Right (Clockwise)
dbr :: SkewbConfiguration
dbr = Skewb (p [[4, 6, 5]], p [[3, 8, 6]], t6 0 0 0 3 1 0, t8 0 0 2 0 0 2 1 2)

-- Corner 8: Lower Back Left (Clockwise)
dbl :: SkewbConfiguration
dbl = Skewb (p [[3, 6, 4]], p [[2, 5, 7]], t6 0 0 0 1 0 3, t8 0 2 0 0 2 0 2 1)


------
-- Permutation representations
------

-- |
-- This sends a configuration to the same permutation of stickers, but with each sticker represented as a number between 1 and 72
-- (including a different number for each orientation of each center sticker).
-- Center stickers (with their 4 orienations each) take values 1 - 24, edge stickers take values 25 - 48, and vertex stickers take values 49 - 72.
--
-- Sticker enumeration:
--
-- >              ---------
-- >             |26    27 |
-- >             |    1    |
-- >             |25    28 |
-- >              ---------
-- >  ---------   ---------   ---------   ---------
-- > |34    41 | |33    44 | |36    43 | |35    42 |
-- > |    3    | |    2    | |    5    | |    4    |
-- > |48    37 | |45    38 | |46    39 | |47    40 |
-- >  ---------   ---------   ---------   ---------
-- >              ---------
-- >             |29    30 |
-- >             |    6    |
-- >             |32    31 |
-- >              ---------
toNumericPermutation :: SkewbConfiguration -> Permutation Integer
toNumericPermutation (Skewb (a, b, xs, ys)) = a' ? b'
  where
    t *!! n = index t 0 n
    a' = pp [(n + 6 * unmod k, (a ?. n) + 6 * unmod (xs *!! n + k)) | n <- [1 .. 6], k <- [0, 1, 2, 3]]
    b' = pp [(n + 8 * unmod k + 24, (b ?. n) + 8 * unmod (ys *!! n + k) + 24) | n <- [1 .. 8], k <- [0, 1, 2]]


------
-- Functions to display ASCII art skewbs
------

color1 :: [Char] -> [Char]
color1 x = "\x1b[31m" ++ x ++ "\x1b[0m"

color2 :: [Char] -> [Char]
color2 x = "\x1b[34m" ++ x ++ "\x1b[0m"

color3 :: [Char] -> [Char]
color3 x = "\x1b[32m" ++ x ++ "\x1b[0m"

color4 :: [Char] -> [Char]
color4 x = "\x1b[36m" ++ x ++ "\x1b[0m"

color5 :: [Char] -> [Char]
color5 x = "\x1b[33m" ++ x ++ "\x1b[0m"

color6 :: [Char] -> [Char]
color6 x = "\x1b[35m" ++ x ++ "\x1b[0m"

colors :: [[Char] -> [Char]]
colors = [color1, color2, color3, color4, color5, color6]

colorLookup :: M.Map Integer ([Char] -> [Char])
colorLookup = M.fromAscList $ zip [1..6] colors


pointup :: String
pointup = " \x25B2 "

pointright = " \x25C0 "

pointdown = " \x25BC "

pointleft = " \x25B6 "

upperleft = " \x25E4 "

upperright = " \x25E5 "

bottomright = " \x25E2 "

bottomleft = " \x25E3 "

shapes :: [String]
shapes = [pointup, pointright, pointdown, pointleft, upperleft, upperright, bottomright, bottomleft]

shapeLookup :: M.Map Integer String
shapeLookup = M.fromAscList $ zip [1..8] shapes

-- Lookup table that assigns to each sticker color a colored string used in its visual representation
-- This contains additional 'colors' used to represent the orientation of center cubies
colorLookup' :: M.Map Integer String
colorLookup' =
  M.fromAscList
    [ (1, color1 " \x25A0 "),
      (2, color2 " \x25A0 "),
      (3, color3 " \x25A0 "),
      (4, color4 " \x25A0 "),
      (5, color5 " \x25A0 "),
      (6, color6 " \x25A0 "),
      (7, color1 " \x25B2 "),
      (8, color1 " \x25C0 "),
      (9, color1 " \x25BC "),
      (10, color1 " \x25B6 "),
      (11, color2 " \x25B2 "),
      (12, color2 " \x25C0 "),
      (13, color2 " \x25BC "),
      (14, color2 " \x25B6 "),
      (15, color3 " \x25B2 "),
      (16, color3 " \x25C0 "),
      (17, color3 " \x25BC "),
      (18, color3 " \x25B6 "),
      (19, color4 " \x25B2 "),
      (20, color4 " \x25C0 "),
      (21, color4 " \x25BC "),
      (22, color4 " \x25B6 "),
      (23, color5 " \x25B2 "),
      (24, color5 " \x25C0 "),
      (25, color5 " \x25BC "),
      (26, color5 " \x25B6 "),
      (27, color6 " \x25B2 "),
      (28, color6 " \x25C0 "),
      (29, color6 " \x25BC "),
      (30, color6 " \x25B6 ")
    ]

-- Lookup table that assigns a sticker color to each cubie face
stickerLookup' :: Integer -> String
stickerLookup' = \case
  1 -> color1 pointup
  2 -> color2 pointup
  3 -> color3 pointup
  4 -> color4 pointup
  5 -> color5 pointup
  6 -> color6 pointup
  7 -> color1 pointright
  8 -> color2 pointright
  9 -> color3 pointright
  10 -> color4 pointright
  11 -> color5 pointright
  12 -> color6 pointright
  13 -> color1 pointdown
  14 -> color2 pointdown
  15 -> color3 pointdown
  16 -> color4 pointdown
  17 -> color5 pointdown
  18 -> color6 pointdown
  19 -> color1 pointleft
  20 -> color2 pointleft
  21 -> color3 pointleft
  22 -> color4 pointleft
  23 -> color5 pointleft
  24 -> color6 pointleft
  25 -> color1 bottomleft
  26 -> color1 upperleft
  27 -> color1 upperright
  28 -> color1 bottomright
  29 -> color6 upperleft
  30 -> color6 upperright
  31 -> color6 bottomright
  32 -> color6 bottomleft
  33 -> color2 upperleft
  34 -> color3 upperleft
  35 -> color4 upperleft
  36 -> color5 upperleft
  37 -> color3 bottomright
  38 -> color2 bottomright
  39 -> color5 bottomright
  40 -> color4 bottomright
  41 -> color3 upperright
  42 -> color4 upperright
  43 -> color5 upperright
  44 -> color2 upperright
  45 -> color2 bottomleft
  46 -> color5 bottomleft
  47 -> color4 bottomleft
  48 -> color3 bottomleft

stickerLookup :: [(Integer, Integer)] -> [String]
stickerLookup = (getSticker <$>)
  where
    getSticker (old, new) = 
      if old <= 24
      then
        getColor new $ getShape new
      else
        getColor old $ getShape new
    getColor = \case
      1 -> color1
      2 -> color2
      3 -> color3
      4 -> color4
      5 -> color5
      6 -> color6
      7 -> color1
      8 -> color2
      9 -> color3
      10 -> color4
      11 -> color5
      12 -> color6
      13 -> color1
      14 -> color2
      15 -> color3
      16 -> color4
      17 -> color5
      18 -> color6
      19 -> color1
      20 -> color2
      21 -> color3
      22 -> color4
      23 -> color5
      24 -> color6
      25 -> color1
      26 -> color1
      27 -> color1
      28 -> color1
      29 -> color6
      30 -> color6
      31 -> color6
      32 -> color6
      33 -> color2
      34 -> color3
      35 -> color4
      36 -> color5
      37 -> color3
      38 -> color2
      39 -> color5
      40 -> color4
      41 -> color3
      42 -> color4
      43 -> color5
      44 -> color2
      45 -> color2
      46 -> color5
      47 -> color4
      48 -> color3
    getShape = \case
      1 -> pointup
      2 -> pointup
      3 -> pointup
      4 -> pointup
      5 -> pointup
      6 -> pointup
      7 -> pointright
      8 -> pointright
      9 -> pointright
      10 -> pointright
      11 -> pointright
      12 -> pointright
      13 -> pointdown
      14 -> pointdown
      15 -> pointdown
      16 -> pointdown
      17 -> pointdown
      18 -> pointdown
      19 -> pointleft
      20 -> pointleft
      21 -> pointleft
      22 -> pointleft
      23 -> pointleft
      24 -> pointleft
      25 -> bottomleft
      26 -> upperleft
      27 -> upperright
      28 -> bottomright
      29 -> upperleft
      30 -> upperright
      31 -> bottomright
      32 -> bottomleft
      33 -> upperleft
      34 -> upperleft
      35 -> upperleft
      36 -> upperleft
      37 -> bottomright
      38 -> bottomright
      39 -> bottomright
      40 -> bottomright
      41 -> upperright
      42 -> upperright
      43 -> upperright
      44 -> upperright
      45 -> bottomleft
      46 -> bottomleft
      47 -> bottomleft
      48 -> bottomleft

--
-- >              ---------
-- >             |26    27 |
-- >             |    1    |
-- >             |25    28 |
-- >              ---------
-- >  ---------   ---------   ---------   ---------
-- > |34    41 | |33    44 | |36    43 | |35    42 |
-- > |    3    | |    2    | |    5    | |    4    |
-- > |48    37 | |45    38 | |46    39 | |47    40 |
-- >  ---------   ---------   ---------   ---------
-- >              ---------
-- >             |29    30 |
-- >             |    6    |
-- >             |32    31 |
-- >              ---------

-- Generates the ASCII art representation of a skewb configuration
-- Takes as input a string for each individual sticker
drawSkewb :: [[Char]] -> [[Char]]
drawSkewb xs = case xs of
  [c10, c20, c30, c40, c50, c60, c11, c21, c31, c41, c51, c61, c12, c22, c32, c42, c52, c62, c13, c23, c33, c43, c53, c63, v10, v20, v30, v40, v50, v60, v70, v80, v11, v21, v31, v41, v51, v61, v71, v81, v12, v22, v32, v42, v52, v62, v72, v82] ->
    [
      "              ---------",
      "             |" ++ v20 ++ "   " ++ v30 ++ "|",
      "             |   " ++ c10 ++ "   |",
      "             |" ++ v10 ++ "   " ++ v40 ++ "|",
      "              ---------",
      "  ---------   ---------   ---------   ---------",
      " |" ++ v21 ++ "   " ++ v12 ++ "| |" ++ v11 ++ "   " ++ v42 ++ "| |" ++ v41 ++ "   " ++ v32 ++ "| |" ++ v31 ++ "   " ++ v22 ++ "|",
      " |   " ++ c30 ++ "   | |   " ++ c20 ++ "   | |   " ++ c50 ++ "   | |   " ++ c40 ++ "   |",
      " |" ++ v82 ++ "   " ++ v51 ++ "| |" ++ v52 ++ "   " ++ v61 ++ "| |" ++ v62 ++ "   " ++ v71 ++ "| |" ++ v72 ++ "   " ++ v81 ++ "|",
      "  ---------   ---------   ---------   ---------",
      "              ---------",
      "             |" ++ v50 ++ "   " ++ v60 ++ "|",
      "             |   " ++ c60 ++ "   |",
      "             |" ++ v80 ++ "   " ++ v70 ++ "|",
      "              ---------"
    ]
  _ -> ["Attempted to draw a skewb without the correct number of stickers :("]

-- | Creates the list of string representing the skewb configuration as ASCII art
-- Takes as input a Map which sends the numbers 1 - 72 to strings. Each string should be three characteres long.
-- These serve as the stickers for the ASCII art. The stickers are enumerated just as in the definition of 'toNumericPermutation'.
showSkewbCustom :: ([(Integer, Integer)] -> [String]) -> SkewbConfiguration -> String
showSkewbCustom lookupFunction g =
  let o = toNumericPermutation $ invert g
      positions = [(n, o ?. n) | n <- [1..48]]
      stickerColors = lookupFunction positions
   in L.intercalate "\n" $ drawSkewb stickerColors

-- | Uses the default assignment of strings to colors:
showSkewb :: SkewbConfiguration -> String
showSkewb = showSkewbCustom $ stickerLookup

-- | Uses the default assignment of strings to colors
printSkewb :: SkewbConfiguration -> IO ()
printSkewb = putStrLn . showSkewb