module Cube where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Modular
import Permutable
import Permutation
import qualified Test.Tasty.QuickCheck as Q

------
-- Defining the CubeConfiguration type
------
type CenterP = Permutation Integer

type EdgeP = Permutation Integer

type VertexP = Permutation Integer

type CenterO = Tuple6 Mod4

type EdgeO = Tuple12 Mod2

type VertexO = Tuple8 Mod3

-- A Rubik's Cube configuration will be represented as a tuple of elements (a,b,c,xs,ys,zs)
-- The first three elements of the tuple are permutations representing how the center, edge, and vertex cubies have been permuted
-- This is based on prior enumerations of the center, edge, and vertex cubies, as shown below.
--
-- Cubie enumeration:
--                 -----------
--                | 2 | 3 | 3 |
--                |---+---+---|
--                | 2 | 1 | 4 |
--                |---+---+---|
--                | 1 | 1 | 4 |
--                 -----------
--   -----------   -----------   -----------   -----------
--  | 2 | 2 | 1 | | 1 | 1 | 4 | | 4 | 4 | 3 | | 3 | 3 | 2 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | 6 | 3 | 5 | | 5 | 2 | 8 | | 8 | 5 | 7 | | 7 | 4 | 6 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | 8 |12 | 5 | | 5 | 9 | 6 | | 6 |10 | 7 | | 7 |11 | 8 |
--   -----------   -----------   -----------   -----------
--                 -----------
--                | 5 | 9 | 6 |
--                |---+---+---|
--                |12 | 6 |10 |
--                |---+---+---|
--                | 8 |11 | 7 |
--                 -----------
--
-- The latter three elements of the tuple are themselves tuples containing integers mod 4, mod 2, and mod 3 respectively.
-- The value in the nth position in each tuple represents the orientation of the nth cubie, relative to the original orientation
-- of the cubie originially in the position it is now in.
-- This is given by an enumeration of the faces of each cubie (except for center cubies, which have 1 face but 4 orientations)
-- For center and vertex cubies, an increment to their orientation value corresponds to a clockwise turn.
-- For edge cubies, an increment to their orientation value (which is mod 2) corresponds to a flip.
--
-- Orientation enumerations:
--                 -----------
--                | 0 | 0 | 0 |
--                |---+---+---|
--                | 0 | * | 0 |
--                |---+---+---|
--                | 0 | 0 | 0 |
--                 -----------
--   -----------   -----------   -----------   -----------
--  | 1 | 1 | 2 | | 1 | 1 | 2 | | 1 | 1 | 2 | | 1 | 1 | 2 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | 1 | * | 1 | | 0 | * | 0 | | 1 | * | 1 | | 0 | * | 0 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | 2 | 1 | 1 | | 2 | 1 | 1 | | 2 | 1 | 1 | | 2 | 1 | 1 |
--   -----------   -----------   -----------   -----------
--                 -----------
--                | 0 | 0 | 0 |
--                |---+---+---|
--                | 0 | * | 0 |
--                |---+---+---|
--                | 0 | 0 | 0 |
--                 -----------
--
-- So if for a given configuration, c(3) = 5 and the 3rd value of zs is 2, then in this configuration, the 0 face of the 3rd vertex
-- cubie is aligned with the original position of the 2 face of the 5th vertex cubie
-- The new position and orientation of the 3rd vertex cubie would look as shown below (the original position is marked with Xs)
--                 -----------
--                |   |   | X |
--                |---+---+---|
--                |   |   |   |
--                |---+---+---|
--                |   |   |   |
--                 -----------
--   -----------   -----------   -----------   -----------
--  |   |   |   | |   |   |   | |   |   | X | | X |   |   |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  |   |   |   | |   |   |   | |   |   |   | |   |   |   |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  |   |   | 2 | | 0 |   |   | |   |   |   | |   |   |   |
--   -----------   -----------   -----------   -----------
--                 -----------
--                | 1 |   |   |
--                |---+---+---|
--                |   |   |   |
--                |---+---+---|
--                |   |   |   |
--                 -----------
--
--
-- With this representation, each possible such tuple represents a valid configuration of the cubies
--
newtype CubeConfiguration = Cube (CenterP, EdgeP, VertexP, CenterO, EdgeO, VertexO) deriving (Eq)

cube :: CenterP -> EdgeP -> VertexP -> CenterO -> EdgeO -> VertexO -> CubeConfiguration
cube = (((((Cube .) .) .) .) .) . (,,,,,)

showCubeConfiguration :: CubeConfiguration -> String
showCubeConfiguration (Cube (a, b, c, xs, ys, zs)) = L.intercalate "\n" [showInline a, showInline b, showInline c, show xs, show ys, show zs]

instance Show CubeConfiguration where
  show = showCubeConfiguration

-- Extra newtype that allows for an alternate, more visual Show instance for cube configurations
newtype ShowCube = ShowCube CubeConfiguration deriving (Eq)

instance Show ShowCube where
  show (ShowCube g) = showCube g

------
-- Instantiating typeclasses
------

-- The set of cube configurations forms a group (and thus a semigroup and monoid)

-- Given two configurations, their position permutations of the product is given by the product of the position permutations
-- The orientation tuples of the product takes into account the natural right action of the permutations on the tuples
-- Specifically, this right action is given by (x_1, x_2, ... x_n ) *? o = (x_{o(1)}, x_{o(2)}, ..., x_{o(n)})
-- This gives the group the structure of a direct product of three wreath products, one for the centers, edges, and vertices
-- Each of these is known as a generalized symmetric group
-- Specifically, it is isomorphic to (Z_4 \wr S_6) X (Z_2 \wr S_12) X (Z_3 wr S_8)
-- This has order 4^6 * 6! * 2^12 * 12! * 3^8 * 8! = 1530664174762362289520640000
-- Neglecting center cubie orientation is equivalent to quotienting by Z_4^6 (which is a normal subgroup)
-- This has order 373697308291592355840000
instance Semigroup CubeConfiguration where
  (<>) (Cube (a1, b1, c1, xs1, ys1, zs1)) (Cube (a2, b2, c2, xs2, ys2, zs2)) = Cube (a, b, c, xs, ys, zs)
    where
      a = a1 * a2
      b = b1 * b2
      c = c1 * c2
      xs = xs1 *? a2 + xs2
      ys = ys1 *? b2 + ys2
      zs = zs1 *? c2 + zs2

-- The identity configuration will be given below
instance Monoid CubeConfiguration where
  mempty = i

-- Groups additionally support an inversion operation
invert :: CubeConfiguration -> CubeConfiguration
invert (Cube (a, b, c, xs, ys, zs)) = Cube (a', b', c', xs', ys', zs')
  where
    a' = a ^- 1
    b' = b ^- 1
    c' = c ^- 1
    xs' = a ?* (- xs)
    ys' = b ?* (- ys)
    zs' = c ?* (- zs)

------
-- Shorthand for the group operations
------

-- Multiplication
infixl 7 #

(#) :: CubeConfiguration -> CubeConfiguration -> CubeConfiguration
x # y = x <> y

-- Commutator
infix 7 >#<

(>#<) :: CubeConfiguration -> CubeConfiguration -> CubeConfiguration
x >#< y = invert x # invert y # x # y

-- Exponent
infixl 8 #^

(#^) :: Integral b => CubeConfiguration -> b -> CubeConfiguration
x #^ 0 = i
x #^ (-1) = invert x
x #^ n
  | even n = (x # x) #^ div n 2
  | otherwise = x # (x # x) #^ div n 2

------
-- Explicitly writing out the cube configurations corresponding to the identity plus the single Turn of each face
------

-- Identity (no change)
i :: CubeConfiguration
i = Cube (1, 1, 1, 0, 0, 0)

-- Up (Clockwise)
u :: CubeConfiguration
u = Cube (1, p [[1, 2, 3, 4]], p [[1, 2, 3, 4]], T6 (1, 0, 0, 0, 0, 0), 0, 0)

-- Up (Counterclockwise)
u' :: CubeConfiguration
u' = Cube (1, p [[1, 4, 3, 2]], p [[1, 4, 3, 2]], T6 (3, 0, 0, 0, 0, 0), 0, 0)

-- Front (Clockwise)
f :: CubeConfiguration
f = Cube (1, p [[1, 8, 9, 5]], p [[1, 4, 6, 5]], T6 (0, 1, 0, 0, 0, 0), T12 (1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0), T8 (1, 0, 0, 2, 2, 1, 0, 0))

-- Front (Counterclockwise)
f' :: CubeConfiguration
f' = Cube (1, p [[1, 5, 9, 8]], p [[1, 5, 6, 4]], T6 (0, 3, 0, 0, 0, 0), T12 (1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0), T8 (1, 0, 0, 2, 2, 1, 0, 0))

-- Left (Clockwise)
l :: CubeConfiguration
l = Cube (1, p [[2, 5, 12, 6]], p [[1, 5, 8, 2]], T6 (0, 0, 1, 0, 0, 0), 0, T8 (2, 1, 0, 0, 1, 0, 0, 2))

-- Left (Counterclockwise)
l' :: CubeConfiguration
l' = Cube (1, p [[2, 6, 12, 5]], p [[1, 2, 8, 5]], T6 (0, 0, 3, 0, 0, 0), 0, T8 (2, 1, 0, 0, 1, 0, 0, 2))

-- Back (Clockwise)
b :: CubeConfiguration
b = Cube (1, p [[3, 6, 11, 7]], p [[2, 8, 7, 3]], T6 (0, 0, 0, 1, 0, 0), T12 (0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0), T8 (0, 2, 1, 0, 0, 0, 2, 1))

-- Back (Counterclockwise)
b' :: CubeConfiguration
b' = Cube (1, p [[3, 7, 11, 6]], p [[2, 3, 7, 8]], T6 (0, 0, 0, 3, 0, 0), T12 (0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0), T8 (0, 2, 1, 0, 0, 0, 2, 1))

-- Right (Clockwise)
r :: CubeConfiguration
r = Cube (1, p [[4, 7, 10, 8]], p [[3, 7, 6, 4]], T6 (0, 0, 0, 0, 1, 0), 0, T8 (0, 0, 2, 1, 0, 2, 1, 0))

-- Right (Counterclockwise)
r' :: CubeConfiguration
r' = Cube (1, p [[4, 8, 10, 7]], p [[3, 4, 6, 7]], T6 (0, 0, 0, 0, 3, 0), 0, T8 (0, 0, 2, 1, 0, 2, 1, 0))

-- Down (Clockwise)
d :: CubeConfiguration
d = Cube (1, p [[9, 10, 11, 12]], p [[5, 6, 7, 8]], T6 (0, 0, 0, 0, 0, 1), 0, 0)

-- Down (Counterclockwise)
d' :: CubeConfiguration
d' = Cube (1, p [[9, 12, 11, 10]], p [[5, 8, 7, 6]], T6 (0, 0, 0, 0, 0, 3), 0, 0)

data Turn = I | U | U' | F | F' | L | L' | B | B' | R | R' | D | D'

instance Show Turn where
  show m =
    case m of
      I -> "I"
      U -> "U"
      U' -> "u"
      F -> "F"
      F' -> "f"
      L -> "L"
      L' -> "l"
      B -> "B"
      B' -> "b"
      R -> "R"
      R' -> "r"
      D -> "D"
      D' -> "d"

turnToConfig :: Turn -> CubeConfiguration
turnToConfig m =
  case m of
    I -> i
    U -> u
    U' -> u'
    F -> f
    F' -> f'
    L -> l
    L' -> l'
    B -> b
    B' -> b'
    R -> r
    R' -> r'
    D -> d
    D' -> d'

turnsToConfig :: [Turn] -> CubeConfiguration
turnsToConfig = L.foldr ((#) . turnToConfig) i

-- This function inverts a basic turn
invertTurn :: Turn -> Turn
invertTurn m =
  case m of
    I -> I
    U -> U'
    U' -> U
    F -> F'
    F' -> F
    L -> L'
    L' -> L
    B -> B'
    B' -> B
    R -> R'
    R' -> R
    D -> D'
    D' -> D
    
invertTurns :: [Turn] -> [Turn]
invertTurns [] = []
invertTurns (m : ms) = invertTurns ms ++ [invertTurn m]

------
-- Configurations of the cube can also be seen as permutations of the set of stickers plus the 4 orientations of each center cubie
-- This manifests as an isomorphism between the group of cube configurations and a subgroup of the permutation group of stickers
-- (conversely, not every permutation of stickers gives a valid configuration of the cube, for example a vertex sticker can never
-- end up in the place of an edge sticker in a valid configuration)
------

-- This sends a configuration to a permutation of the stickers, where each sticker is represented as a tuple (X, n, m), where
-- X encodes whether it is a center, edge, or vertex cubie, (taking the values 'C', 'E', or 'V', respectively, n represents the cubie
-- the sticker is attached to, and m represents the face of that cubie that the sticker is attached to.
-- For center cubies, m represents the orientation of the sticker
-- Although m should technically be an integer mod 4, 2, or 3, in order to make the type consistent it is stored as a plain integer
toPermutation :: CubeConfiguration -> Permutation (Char, Integer, Integer)
toPermutation (Cube (a, b, c, xs, ys, zs)) = a' * b' * c'
  where
    a' = pp [(('C', n, k), ('C', a ?. n, toIntegral $ xs *! n + fromInteger k)) | n <- [1 .. 6], k <- [0 .. 3]]
    b' = pp [(('E', n, k), ('E', b ?. n, toIntegral $ ys *! n + fromInteger k)) | n <- [1 .. 12], k <- [0 .. 1]]
    c' = pp [(('V', n, k), ('V', c ?. n, toIntegral $ zs *! n + fromInteger k)) | n <- [1 .. 8], k <- [0 .. 2]]

fromPermutation :: Permutation (Char, Integer, Integer) -> CubeConfiguration
fromPermutation o = Cube (a, b, c, xs, ys, zs)
  where
    grabClass = \((c, _, _), _) -> c
    grabPerm = \((_, n, _), (_, n', _)) -> (n, n')
    grabShift = \(_, _, k) -> k
    a = pp $ map grabPerm $ filter ((== 'C') . grabClass) $ toPairs o
    b = pp $ map grabPerm $ filter ((== 'E') . grabClass) $ toPairs o
    c = pp $ map grabPerm $ filter ((== 'V') . grabClass) $ toPairs o
    xs = fromList [fromInteger $ grabShift $ o ?. ('C', n, 0) | n <- [1 .. 6]]
    ys = fromList [fromInteger $ grabShift $ o ?. ('E', n, 0) | n <- [1 .. 12]]
    zs = fromList [fromInteger $ grabShift $ o ?. ('V', n, 0) | n <- [1 .. 8]]

-- This sends a configuration to the same permutation of stickers, but with each sticker represented as a number between 1 and 72
-- (including a different number for each orientation of each center sticker)
-- Center stickers (with their 4 orienations each) take values 1 - 24, edge stickers take values 25 - 48, and vertex stickers take values 49 - 72
--
-- Sticker enumeration:
--                 -----------
--                |50 |27 |51 |
--                |---+---+---|
--                |26 | 1 |28 |
--                |---+---+---|
--                |49 |25 |52 |
--                 -----------
--   -----------   -----------   -----------   -----------
--  |58 |38 |65 | |57 |37 |68 | |60 |40 |67 | |59 |39 |66 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  |42 | 3 |41 | |29 | 2 |32 | |44 | 5 |43 | |31 | 4 |30 |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  |72 |48 |61 | |69 |45 |62 | |70 |46 |63 | |71 |47 |64 |
--   -----------   -----------   -----------   -----------
--                 -----------
--                |53 |33 |54 |
--                |---+---+---|
--                |36 | 6 |34 |
--                |---+---+---|
--                |56 |35 |55 |
--                 -----------
--
toGenericPermutation :: CubeConfiguration -> Permutation Integer
toGenericPermutation (Cube (a, b, c, xs, ys, zs)) = a' * b' * c'
  where
    a' = pp [(n + 6 * k, (a ?. n) + 6 * toIntegral (xs *! n + fromInteger k)) | n <- [1 .. 6], k <- [0 .. 3]]
    b' = pp [(n + 12 * k + 24, (b ?. n) + 12 * toIntegral (ys *! n + fromInteger k) + 24) | n <- [1 .. 12], k <- [0 .. 1]]
    c' = pp [(n + 8 * k + 48, (c ?. n) + 8 * toIntegral (zs *! n + fromInteger k) + 48) | n <- [1 .. 8], k <- [0 .. 2]]

fromGenericPermutation :: Permutation Integer -> CubeConfiguration
fromGenericPermutation o = Cube (a, b, c, xs, ys, zs)
  where
    a = pp [(n, ((o ?. n) - 1) `mod` 6 + 1) | n <- [1 .. 6]]
    b = pp [(n, ((o ?. (n + 24)) - 25) `mod` 12 + 1) | n <- [1 .. 12]]
    c = pp [(n, ((o ?. (n + 48)) - 49) `mod` 8 + 1) | n <- [1 .. 8]]
    xs = fromList $ [fromInteger $ (o ?. n) `div` 6 - n `div` 6 | n <- [1 .. 6]]
    ys = fromList $ [fromInteger $ (o ?. (n + 24)) `div` 12 - (n + 24) `div` 12 | n <- [1 .. 12]]
    zs = fromList $ [fromInteger $ (o ?. (n + 48)) `div` 8 - (n + 48) `div` 8 | n <- [1 .. 8]]

------
-- Useful group functions
------

-- Gives the order of an element within the group of configurations (or, equivalently, within the group of sticker permutations)
-- Takes into account center orientations
order :: CubeConfiguration -> Int
order = orderE . toPermutation

-- Neglects center orientations
order' :: CubeConfiguration -> Int
order' (Cube (a, b, c, xs, ys, zs)) = orderE . toPermutation $ Cube (a, b, c, 0, ys, zs)

------
-- Solving the Rubik's Cube
------

-- Two elements are similar when one can be obtained from the other via a sequence of basic turns
-- That is to say, g is similar to h when g # h^{-1} is an element of <u, f, l, b, r, d>
-- This is an equivalence relation
-- The characterization of similarity given by this function can be proved equivalent to the one above
-- This characterization of similarity is a large motivating factor in the representation of cube configurations used here
isSimilarTo :: CubeConfiguration -> CubeConfiguration -> Bool
isSimilarTo (Cube (a, b, c, xs, ys, zs)) (Cube (a', b', c', xs', ys', zs')) = t1 && t2 && t3 && t4 && t5
  where
    t1 = a == a'
    t2 = sgn b * sgn c == sgn b' * sgn c'
    t3 = sgn b * (-1) ^ toIntegral (sum xs) == sgn b' * (-1) ^ toIntegral (sum xs')
    t4 = sum ys == sum ys'
    t5 = sum zs == sum zs'

-- An element is legal if it is an element of <u, f, l, b, r, d>
-- This is equivalent to being similar to the identity
-- An equivalent characterization is that Cube (a,b,c,xs,ys,zs) is legal exactly when
-- a = 1  (The centers are unmoved)
-- sgn b = sgn c (For each pair of edge cubies swapped, a pair of vertex cubies is also swapped and vice versa)
-- sgn b = (-1)^(sum xs) (For each pair of edge cubies swapped, a center cubie is turned once and vice versa)
-- [(-1)^(sum xs) since sum xs is in Z_4]
-- sum ys = 0 (Edge cubies are flipped in pairs)
-- sum zs = 0 (Vertex cubies are turned in opposite pairs--if one is turned cw, another is turned ccw)
-- This means that the Legal Cube Group is isomorphic to the Cube Group quotiented by
-- S_6
-- Three copies of Z_2
-- One copy of Z_3
-- And so it has order (4^6 * 6! * 2^12 * 12! * 3^8 * 8!) / (6! * 2^3 * 3) = 88580102706155225088000
-- Ignoring the center orientations means quotienting by Z_4^6 (and no longer quotienting by one copy of Z_2)
-- This gives order (4^6 * 6! * 2^12 * 12! * 3^8 * 8!) / (4^6 * 6! * 2^2 * 3) = 43252003274489856000
isLegal :: CubeConfiguration -> Bool
isLegal = isSimilarTo i

-- Given a legal configuration, creates a sequence of basic Turn that produce that configuration
generate :: CubeConfiguration -> Maybe [Turn]
generate g
  | isLegal g = Just $ generate' g
  | otherwise = Nothing
  where
    -- The centers are never out of position in a legal configuration
    -- At the end, orients the center cube of the U face (the orientation must be 0 or 2 mod 4)
    -- The centers are oriented at the beginning, but the U face may get turned again while
    -- positioning the edges
    generate' g0 =
      let ms1 = orientCenters g0
          g1 = g0 # invert (turnsToConfig ms1)
          ms2 = positionEdges g1
          g2 = g1 # invert (turnsToConfig ms2)
          ms3 = orientEdges g2
          g3 = g2 # invert (turnsToConfig ms3)
          ms4 = positionVertices g3
          g4 = g3 # invert (turnsToConfig ms4)
          ms5 = orientVertices g4
          g5 = g4 # invert (turnsToConfig ms4)
          ms6 = orientLastCenter g5
       in ms6 ++ ms5 ++ ms4 ++ ms3 ++ ms2 ++ ms1

-- Given a legal configuration, creates a sequence of basic Turn that produce the inverse of that configuration
solve :: CubeConfiguration -> Maybe [Turn]
solve = fmap invertTurns . generate

orientCenters :: CubeConfiguration -> [Turn]
orientCenters (Cube (_, _, _, xs, _, _)) = concatMap modToSequence (toPairList xs)
  where
    modToSequence (n, k) = replicate (fromInteger $ toIntegral k) $
      case n of
        1 -> U
        2 -> F
        3 -> L
        4 -> B
        5 -> R
        6 -> D
        _ -> I

positionEdges :: CubeConfiguration -> [Turn]
positionEdges (Cube (_, b, _, _, _, _)) = concatMap transpositionToSequence (transpositionDecomposition 1 b)
  where
    x = [U, R', U, U, R, U, R', U, R]
    y = [L, U, L', U, L, U, U, L', U]
    transpositionToSequence ts =
      case ts of
        [1, 2] -> x
        [1, 3] -> [B', L'] ++ x ++ [L, B]
        [1, 4] -> y
        [1, 5] -> [L] ++ x ++ [L']
        [1, 6] -> [L'] ++ x ++ [L]
        [1, 7] -> [R] ++ y ++ [R']
        [1, 8] -> [R'] ++ y ++ [R]
        [1, 9] -> [D', R, R] ++ y ++ [R, R, D]
        [1, 10] -> [R, R] ++ y ++ [R, R]
        [1, 11] -> [B', R] ++ y ++ [R', B]
        [1, 12] -> [L, L] ++ x ++ [L, L]
        _ -> []

orientEdges :: CubeConfiguration -> [Turn]
orientEdges (Cube (_, _, _, _, ys, _)) = concatMap modToSequence $ tail $ toPairList ys
  where
    x = [L, U', L', U, L', F, L, F']
    y = [R', U, R, U', R, F', R', F]
    modToSequence (n, k) = concat $
      replicate (fromInteger $ toIntegral k) $
        case n of
          2 -> x
          3 -> [B', L'] ++ x ++ [L, B]
          4 -> y
          5 -> [L] ++ x ++ [L']
          6 -> [L'] ++ x ++ [L]
          7 -> [R] ++ y ++ [R']
          8 -> [R'] ++ y ++ [R]
          9 -> [D', R, R] ++ y ++ [R, R, D]
          10 -> [R, R] ++ y ++ [R, R]
          11 -> [B', R] ++ y ++ [R', B]
          12 -> [L, L] ++ x ++ [L, L]
          _ -> []

positionVertices :: CubeConfiguration -> [Turn]
positionVertices (Cube (_, _, c, _, _, _)) = concatMap threeCycleToSequence $ threeCycleDecomposition 1 2 c
  where
    x = [L, F', L', F, L, F', L', F, L, F', L', F, U, U, L, F', L', F, L, F', L', F, L, F', L', F, U', L, F', L', F, L, F', L', F, L, F', L', F, U', L, F', L', F, L, F', L', F, L, F', L', F]
    y = [F', L, F, L', F', L, F, L', F', L, F, L', U, F', L, F, L', F', L, F, L', F', L, F, L', U, F', L, F, L', F', L, F, L', F', L, F, L', U, U, F', L, F, L', F', L, F, L', F', L, F, L']
    threeCycleToSequence ts = case ts of
      [1, 2, 3] -> x
      [2, 1, 3] -> y
      [1, 2, 4] -> [R'] ++ x ++ [R]
      [2, 1, 4] -> [R'] ++ y ++ [R]
      [1, 2, 5] -> [D', R, R] ++ x ++ [R, R, D]
      [2, 1, 5] -> [D', R, R] ++ y ++ [R, R, D]
      [1, 2, 6] -> [R, R] ++ x ++ [R, R]
      [2, 1, 6] -> [R, R] ++ y ++ [R, R]
      [1, 2, 7] -> [R] ++ x ++ [R']
      [2, 1, 7] -> [R] ++ y ++ [R']
      [1, 2, 8] -> [D, R] ++ x ++ [R', D']
      [2, 1, 8] -> [D, R] ++ y ++ [R', D']
      _ -> []

orientVertices :: CubeConfiguration -> [Turn]
orientVertices (Cube (_, _, _, _, _, zs)) = concatMap modToSequence $ tail $ toPairList zs
  where
    x = [U, L, F', L', F, L, F', L', F, U', F', L, F, L', F', L, F, L']
    y = [U, R', F, R, F', R', F, R, F', U', F, R', F', R, F, R', F', R]
    modToSequence (n, k) = concat $
      replicate (fromInteger $ toIntegral k) $
        case n of
          2 -> x
          3 -> [B'] ++ x ++ [B]
          4 -> y
          5 -> [D', R'] ++ y ++ [R, D]
          6 -> [R'] ++ y ++ [R]
          7 -> [B, B] ++ x ++ [B, B]
          8 -> [B] ++ x ++ [B']
          _ -> []

orientLastCenter :: CubeConfiguration -> [Turn]
orientLastCenter (Cube (_, _, _, xs, _, _)) = case xs of
  T6 (2, 0, 0, 0, 0, 0) -> [U, R', L', U, U, L, R, U, R', L', U, U, L, R]
  _ -> []

------
-- Functions to display ASCII art cubes
------

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

--  What the Cube looks like in its default state:
--                 -----------
--                |   |   |   |
--                |---+---+---|
--                |   |   |   |
--                |---+---+---|
--                |   |   |   |
--                 -----------
--   -----------   -----------   -----------   -----------
--  | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
--   -----------   -----------   -----------   -----------
--                 -----------
--                | ~ | ~ | ~ |
--                |---+---+---|
--                | ~ | ~ | ~ |
--                |---+---+---|
--                | ~ | ~ | ~ |
--                 -----------
--

-- Generates the ASCII art representation of a cube configuration
-- Takes as input a string for each sticker
drawCube :: [[Char]] -> [[Char]]
drawCube xs = case xs of
  [c10, c20, c30, c40, c50, c60, c11, c21, c31, c41, c51, c61, c12, c22, c32, c42, c52, c62, c13, c23, c33, c43, c53, c63, e10, e20, e30, e40, e50, e60, e70, e80, e90, e100, e110, e120, e11, e21, e31, e41, e51, e61, e71, e81, e91, e101, e111, e121, v10, v20, v30, v40, v50, v60, v70, v80, v11, v21, v31, v41, v51, v61, v71, v81, v12, v22, v32, v42, v52, v62, v72, v82] ->
    [ "                 -----------                             ",
      "                |" ++ v20 ++ "|" ++ e30 ++ "|" ++ v30 ++ "|                            ",
      "                |---+---+---|                            ",
      "                |" ++ e20 ++ "|" ++ c10 ++ "|" ++ e40 ++ "|                            ",
      "                |---+---+---|                            ",
      "                |" ++ v10 ++ "|" ++ e10 ++ "|" ++ v40 ++ "|                            ",
      "                 -----------                             ",
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

-- Creates the list of string representing the cube configuration as ASCII art
showCubeCustom :: M.Map Integer Integer -> M.Map Integer String -> CubeConfiguration -> String
showCubeCustom stickerLookupMap colorLookupMap g =
  let o = toGenericPermutation g
      stickerColors = M.elems $ M.fromList [(o ?. n, colorLookupMap M.! x) | (n, x) <- M.toList stickerLookupMap]
   in L.intercalate "\n" $ drawCube stickerColors

showCube :: CubeConfiguration -> String
showCube = showCubeCustom stickerLookup colorLookup

-- Displays a configuration to STDOut
displayConfigCustom :: M.Map Integer Integer -> M.Map Integer String -> CubeConfiguration -> IO ()
displayConfigCustom stickerLookupMap colorLookupMap x = putStrLn $ showCubeCustom stickerLookupMap colorLookupMap x

displayConfig :: CubeConfiguration -> IO ()
displayConfig x = putStrLn $ showCube x

------
-- Displaying the Cube with oriented centers
------

-- Lookup table that assigns to each color a string used in its visual representation
-- This contains additional 'colors' used to represent the orientation of center cubies
colorLookupOC :: M.Map Integer String
colorLookupOC =
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

-- Lookup table that assigns a sticker color to each cubie face
stickerLookupOC :: M.Map Integer Integer
stickerLookupOC =
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

--  What the Cube looks like in its default state:
--                 -----------
--                |   |   |   |
--                |---+---+---|
--                |   |^ ^|   |
--                |---+---+---|
--                |   |   |   |
--                 -----------
--   -----------   -----------   -----------   -----------
--  | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | X |^X^| X | |:::|^:^|:::| |###|^#^|###| | o |^o^| o |
--  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
--  | X | X | X | |:::|:::|:::| |###|###|###| | o | o | o |
--   -----------   -----------   -----------   -----------
--                 -----------
--                | ~ | ~ | ~ |
--                |---+---+---|
--                | ~ |^~^| ~ |
--                |---+---+---|
--                | ~ | ~ | ~ |
--                 -----------
--

displayConfigOCube :: CubeConfiguration -> IO ()
displayConfigOCube = displayConfigCustom stickerLookupOC colorLookupOC

------
-- Testing Instances
------

instance Q.Arbitrary Turn where
  arbitrary = Q.elements [I, U, U', F, F', L, L', B, B', R, R', D, D']

-- Arbitrary permutations of cubies are just random permutations of the corresponding subset of Integer
arbCenterP :: Q.Gen CenterP
arbCenterP = permutationOf [1 .. 6]

arbEdgeP :: Q.Gen EdgeP
arbEdgeP = permutationOf [1 .. 12]

arbVertexP :: Q.Gen VertexP
arbVertexP = permutationOf [1 .. 8]

-- For orientations, arbitrary elements can be generated via the instances defined for their components
arbCenterO :: Q.Gen CenterO
arbCenterO = Q.arbitrary

arbEdgeO :: Q.Gen EdgeO
arbEdgeO = Q.arbitrary

arbVertexO :: Q.Gen VertexO
arbVertexO = Q.arbitrary

{-
>>> Q.generate $ ShowCube <$> (Q.arbitrary :: Q.Gen CubeConfiguration)
                 -----------
                |   | o |:::|
                |---+---+---|
                | X | X |   |
                |---+---+---|
                | o | ~ |   |
                 -----------
   -----------   -----------   -----------   -----------
  |###| ~ |   | |###| o | X | |:::| o | ~ | |###| X |:::|
  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
  |   |   |   | |###| o |   | | X |:::|###| |:::| ~ |:::|
  |---+---+---| |---+---+---| |---+---+---| |---+---+---|
  | ~ |:::| ~ | | o | ~ |:::| | ~ | ~ | o | |   | o | o |
   -----------   -----------   -----------   -----------
                 -----------
                | X |:::| X |
                |---+---+---|
                | X |###|###|
                |---+---+---|
                |###|###| X |
                 -----------
-}
instance Q.Arbitrary CubeConfiguration where
  arbitrary = cube <$> arbCenterP <*> arbEdgeP <*> arbVertexP <*> arbCenterO <*> arbEdgeO <*> arbVertexO
