-- |
-- Module      :  ClassicCube
-- Copyright   :  (c) Grant Goodman 2021
-- Description :  Example using the Rubik's Cube Group
-- License     :  MIT
-- Maintainer  :  zornslemonade@gmail.com
-- Portability :  Experimental
--
-- The classic Rubik's Cube does not distinguish different orientations for the center cubies
-- 
-- In total, the classic Rubik's Cube Group has the structure of \(S_6 \times (\mathbb{Z}_2 \wr S_{12}) \times (\mathbb{Z}_3 \wr S_8)\).
--
-- Thus it has order 6! * 2^12 * 12! * 3^8 * 8! = 373697308291592355840000.
module Cube.ClassicCube
  ( makeClassic,
    orderClassic,
    showClassicCubeConfig,
    ASCIIClassicCube,
    showClassicCube,
  )
where

import Cube
  ( CubeConfiguration (..),
    order,
    showCubeConfig,
    showCubeCustom,
  )
import qualified Data.Function as F
import qualified Data.Map as M

-- |Ignore center orientations
makeClassic :: CubeConfiguration -> CubeConfiguration
makeClassic (Cube (a, b, c, xs, ys, zs)) = Cube (a, b, c, 0, ys, zs)

-- |Compute the order of a cube configuration ignoring center orientations
orderClassic :: CubeConfiguration -> Int
orderClassic = order . makeClassic

-- |Show a cube configuration ignoring center orientations
showClassicCubeConfig :: CubeConfiguration -> String
showClassicCubeConfig = showCubeConfig . makeClassic

-- |Create ASCII art for the cube ignoring center orientations
showClassicCubeCustom :: M.Map Integer String -> CubeConfiguration -> String
showClassicCubeCustom m = showCubeCustom m . makeClassic

-- |
-- What the Cube looks like in its default state:
--
-- >                -----------
-- >               |   |   |   |
-- >               |---+---+---|
-- >               |   |   |   |
-- >               |---+---+---|
-- >               |   |   |   |
-- >                -----------
-- >  -----------   -----------   -----------   -----------
-- > | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
-- > |---+---+---| |---+---+---| |---+---+---| |---+---+---|
-- > | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
-- > |---+---+---| |---+---+---| |---+---+---| |---+---+---|
-- > | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
-- >  -----------   -----------   -----------   -----------
-- >                -----------
-- >               | ~ | ~ | ~ |
-- >               |---+---+---|
-- >               | ~ | ~ | ~ |
-- >               |---+---+---|
-- >               | ~ | ~ | ~ |
-- >                -----------
--
newtype ASCIIClassicCube = ShowClassicCube CubeConfiguration

instance Eq ASCIIClassicCube where
  ShowClassicCube g == ShowClassicCube h = makeClassic g == makeClassic h

instance Show ASCIIClassicCube where
  show (ShowClassicCube g) = showClassicCube g

-- |Create ASCII art for the cube ignoring center orientations
showClassicCube :: CubeConfiguration -> String
showClassicCube = showClassicCubeCustom (M.fromAscList [(x, colorLookup M.! (stickerLookup M.! x)) | x <- [1 .. 72]])

-- Lookup table that assigns to each color a string used in its visual representation
-- By default, sticker do not track the orientation of center cubies
colorLookup :: M.Map Integer String
colorLookup =
  M.fromAscList
    [ (1, "   "),
      (2, ":::"),
      (3, " X "),
      (4, " o "),
      (5, "###"),
      (6, " ~ ")
    ]

-- Lookup table that assigns a sticker color to each cubie face
stickerLookup :: M.Map Integer Integer
stickerLookup =
  M.fromAscList
    [ (1, 1),
      (2, 2),
      (3, 3),
      (4, 4),
      (5, 5),
      (6, 6),
      (7, 1),
      (8, 2),
      (9, 3),
      (10, 4),
      (11, 5),
      (12, 6),
      (13, 1),
      (14, 2),
      (15, 3),
      (16, 4),
      (17, 5),
      (18, 6),
      (19, 1),
      (20, 2),
      (21, 3),
      (22, 4),
      (23, 5),
      (24, 6),
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
