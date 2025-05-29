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
import Data.Group
import qualified Data.List as L
import Data.Monoid
import Data.Semigroup
import Modular (Mod3, Mod4)
import Number.GaloisField2p32m5 (base)
import NumericPrelude
    ( filter,
      fst,
      map,
      ($),
      Eq,
      Ord,
      Show(show),
      Bool(..),
      String,
      Integer,
      Maybe(..),
      uncurry,
      all,
      (.),
      (+),
      fromInteger,
      ifThenElse )
import Action
import Permutation hiding (i)
import qualified Permutation as P
import Tuple
import TwistyPuzzle hiding (i)
import qualified Data.Function as F
import Control.Applicative

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
  -- Configurations of the cube can also be seen as permutations of the set of stickers (where the 4 orientations of each center
  -- cubie sticker are considered distinct).
  -- This manifests as a monomorphism from the group of cube configurations into the permutation group of stickers.
  -- Conversely, not every permutation of stickers gives a valid configuration of the cube, for example a vertex sticker can never
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
-- Functions to display ASCII art cubes
------

-- Lookup table that assigns to each sticker color an uncolored string used in its visual representation
-- This contains additional 'colors' used to represent the orientation of center cubies
asciiLookup :: M.Map Integer String
asciiLookup =
  M.fromAscList
    [ (1, "   "),
      (2, ":::"),
      (3, " X "),
      (4, " o "),
      (5, "###"),
      (6, " ~ "),
      (7, "^ ^"),
      (8, "< <"),
      (9, "v v"),
      (10, "> >"),
      (11, "^:^"),
      (12, "<:<"),
      (13, "v:v"),
      (14, ">:>"),
      (15, "^X^"),
      (16, "<X<"),
      (17, "vXv"),
      (18, ">X>"),
      (19, "^o^"),
      (20, "<o<"),
      (21, "vov"),
      (22, ">o>"),
      (23, "^#^"),
      (24, "<#<"),
      (25, "v#v"),
      (26, ">#>"),
      (27, "^~^"),
      (28, "<~<"),
      (29, "v~v"),
      (30, ">~>")
    ]

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

-- Lookup table that assigns to each sticker color a colored string used in its visual representation
-- This contains additional 'colors' used to represent the orientation of center cubies
colorLookup :: M.Map Integer String
colorLookup =
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
stickerLookup :: M.Map Integer Integer
stickerLookup =
  M.fromAscList
    [ (1, 7),
      (2, 11),
      (3, 15),
      (4, 19),
      (5, 23),
      (6, 27),
      (7, 8),
      (8, 12),
      (9, 16),
      (10, 20),
      (11, 24),
      (12, 28),
      (13, 9),
      (14, 13),
      (15, 17),
      (16, 21),
      (17, 25),
      (18, 29),
      (19, 10),
      (20, 14),
      (21, 18),
      (22, 22),
      (23, 26),
      (24, 30),
      (25, 1),
      (26, 1),
      (27, 1),
      (28, 1),
      (29, 2),
      (30, 4),
      (31, 4),
      (32, 2),
      (33, 6),
      (34, 6),
      (35, 6),
      (36, 6),
      (37, 2),
      (38, 3),
      (39, 4),
      (40, 5),
      (41, 3),
      (42, 3),
      (43, 5),
      (44, 5),
      (45, 2),
      (46, 5),
      (47, 4),
      (48, 3),
      (49, 1),
      (50, 1),
      (51, 1),
      (52, 1),
      (53, 6),
      (54, 6),
      (55, 6),
      (56, 6),
      (57, 2),
      (58, 3),
      (59, 4),
      (60, 5),
      (61, 3),
      (62, 2),
      (63, 5),
      (64, 4),
      (65, 3),
      (66, 4),
      (67, 5),
      (68, 2),
      (69, 2),
      (70, 5),
      (71, 4),
      (72, 3)
    ]

-- Generates the ASCII art representation of a cube configuration
-- Takes as input a string for each individual sticker
drawCube :: [[Char]] -> [[Char]]
drawCube xs = case xs of
  [c10, c20, c30, c40, c50, c60, c11, c21, c31, c41, c51, c61, c12, c22, c32, c42, c52, c62, c13, c23, c33, c43, c53, c63, e10, e20, e30, e40, e50, e60, e70, e80, e90, e100, e110, e120, e11, e21, e31, e41, e51, e61, e71, e81, e91, e101, e111, e121, v10, v20, v30, v40, v50, v60, v70, v80, v11, v21, v31, v41, v51, v61, v71, v81, v12, v22, v32, v42, v52, v62, v72, v82] ->
    [ "                 ---------                             ",
      "                | \x25E4 \x25E2 \x25E3 \x25E5 |                            ",
      "                | \x25E2   \x25FC \x25E3 |                            ",
      "                | \x25E5     \x25E4 |                            ",
      "                | \x25E3 \x25E5 \x25E4 \x25E2 |                            ",
      "                 ---------                             ",
      "   -----------   -----------   -----------   ----------- ",
      "  |" ++ v21 ++ "|" ++ e21 ++ "|" ++ v12 ++ "| |" ++ v11 ++ "|" ++ e11 ++ "|" ++ v42 ++ "| |" ++ v41 ++ "|" ++ e41 ++ "|" ++ v32 ++ "| |" ++ v31 ++ "|" ++ e31 ++ "|" ++ v22 ++ "|",
      "  |---+---+---| |---+---+---| |---+---+---| |---+---+---|",
      "  |" ++ e61 ++ "|" ++ c30 ++ "|" ++ e51 ++ "| |" ++ e50 ++ "|" ++ c20 ++ "|" ++ e80 ++ "| |" ++ e81 ++ "|" ++ c50 ++ "|" ++ e71 ++ "| |" ++ e70 ++ "|" ++ c40 ++ "|" ++ e60 ++ "|",
      "  |---+---+---| |---+---+---| |---+---+---| |---+---+---|",
      "  |" ++ v82 ++ "|" ++ e121 ++ "|" ++ v51 ++ "| |" ++ v52 ++ "|" ++ e91 ++ "|" ++ v61 ++ "| |" ++ v62 ++ "|" ++ e101 ++ "|" ++ v71 ++ "| |" ++ v72 ++ "|" ++ e111 ++ "|" ++ v81 ++ "|",
      "   -----------   -----------   -----------   ----------- ",
      "                 -----------                             ",
      "                |" ++ v50 ++ "|" ++ e90 ++ "|" ++ v60 ++ "|                            ",
      "                |---+---+---|                            ",
      "                |" ++ e120 ++ "|" ++ c60 ++ "|" ++ e100 ++ "|                            ",
      "                |---+---+---|                            ",
      "                |" ++ v80 ++ "|" ++ e110 ++ "|" ++ v70 ++ "|                            ",
      "                 -----------                             "
    ]
  _ -> ["Attempted to draw a cube without the correct number of stickers :("]

-- | Creates the list of string representing the cube configuration as ASCII art
-- Takes as input a Map which sends the numbers 1 - 72 to strings. Each string should be three characteres long.
-- These serve as the stickers for the ASCII art. The stickers are enumerated just as in the definition of 'toNumericPermutation'.
showCubeCustom :: M.Map Integer String -> CubeConfiguration -> String
showCubeCustom lookupMap g =
  let o = toNumericPermutation g
      stickerColors = M.elems $ M.fromList [(o ?. n, x) | (n, x) <- M.toList lookupMap]
   in L.intercalate "\n" $ drawCube stickerColors

-- | Uses the default assignment of strings to colors
showCube :: CubeConfiguration -> String
showCube = showCubeCustom $ M.fromAscList [(x, colorLookup M.! (stickerLookup M.! x)) | x <- [1 .. 72]]

-- | Uses the default assignment of strings to colors
printCube :: CubeConfiguration -> IO ()
printCube = putStrLn . showCube