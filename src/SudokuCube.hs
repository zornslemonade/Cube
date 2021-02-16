module SudokuCube where

import Cube ( displayConfigCustom )
import qualified Data.Map as M

-- Lookup table that assigns to each color a string used in its visual representation
-- For a Sudoku cube, these are just the numbers 1 to 9
-- The center orientations are not distinguished
colorLookupSudoku :: M.Map Integer String
colorLookupSudoku =
  M.fromAscList
    [ (1, " 1 "),
      (2, " 2 "),
      (3, " 3 "),
      (4, " 4 "),
      (5, " 5 "),
      (6, " 6 "),
      (7, " 7 "),
      (8, " 8 "),
      (9, " 9 ")
    ]

-- Lookup table that assigns a sticker color to each cubie face
-- For a Sudoku cube, these are assigned such that the stickers on each face
-- make a valid Sudoku puzzle
stickerLookupSudoku :: M.Map Integer Integer
stickerLookupSudoku =
  M.fromAscList
    [ (1, 1),
      (2, 5),
      (3, 7),
      (4, 1),
      (5, 4),
      (6, 6),
      (7, 1),
      (8, 5),
      (9, 7),
      (10, 1),
      (11, 4),
      (12, 6),
      (13, 1),
      (14, 5),
      (15, 7),
      (16, 1),
      (17, 4),
      (18, 6),
      (19, 1),
      (20, 5),
      (21, 7),
      (22, 1),
      (23, 4),
      (24, 6),
      (25, 6),
      (26, 2),
      (27, 8),
      (28, 9),
      (29, 7),
      (30, 5),
      (31, 6),
      (32, 1),
      (33, 5),
      (34, 3),
      (35, 8),
      (36, 4),
      (37, 3),
      (38, 4),
      (39, 9),
      (40, 7),
      (41, 3),
      (42, 8),
      (43, 8),
      (44, 2),
      (45, 2),
      (46, 3),
      (47, 2),
      (48, 9),
      (49, 5),
      (50, 3),
      (51, 7),
      (52, 4),
      (53, 1),
      (54, 2),
      (55, 9),
      (56, 7),
      (57, 4),
      (58, 1),
      (59, 3),
      (60, 9),
      (61, 5),
      (62, 8),
      (63, 6),
      (64, 4),
      (65, 2),
      (66, 7),
      (67, 1),
      (68, 6),
      (69, 9),
      (70, 5),
      (71, 8),
      (72, 6)
    ]

--  What the Cube looks like in its default state:
--                 -----------
--                | 3 | 8 | 7 |
--                |---+---+---|
--                | 2 | 1 | 9 |
--                |---+---+---|
--                | 5 | 6 | 4 |
--                 -----------
--   -----------   -----------   -----------   -----------
--  | 1 | 4 | 2 | | 4 | 3 | 6 | | 9 | 7 | 1 | | 3 | 9 | 7 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | 8 | 7 | 3 | | 7 | 5 | 1 | | 2 | 4 | 8 | | 6 | 1 | 5 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | 6 | 9 | 5 | | 9 | 2 | 8 | | 5 | 3 | 6 | | 8 | 2 | 4 |
--   -----------   -----------   -----------   -----------
--                 -----------
--                | 1 | 5 | 2 |
--                |---+---+---|
--                | 4 | 6 | 3 |
--                |---+---+---|
--                | 7 | 8 | 9 |
--                 -----------

displayConfigSudoku = displayConfigCustom stickerLookupSudoku colorLookupSudoku