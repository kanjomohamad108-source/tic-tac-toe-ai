module Logic where -- do NOT CHANGE export of module
--Diese Hausaaufgabe wurde unter Verwendung von KI-Werkzeugen (insbesondere ChatGPT und Gemini) bearbeitet.

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char

import Board
import Data.Char (chr, ord)
import Data.Maybe ()

data Move = Move {start :: Pos, target :: Pos}

instance Show Move where
  show (Move (Pos startC startR) (Pos targetC targetR)) = [startC] ++ show startR ++ "-" ++ [targetC] ++ show targetR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1)) (Move (Pos sc2 sr2) (Pos tc2 tr2)) =
    sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2

buildMove :: String -> Maybe Move
buildMove "" = Nothing
buildMove s = case break (== '-') s of
  (a, '-' : b) -> Just (Move (buildPos a) (buildPos b))
  _ -> error "Invalid move format"

-- ########################################################################################################
-- ################## pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]        ##################
-- ################## - 5 Functional Points                                              ##################
-- ########################################################################################################

pawnMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
pawnMoves board player pos lastMove
  | cellAt board pos /= Pawn = []
  | not (inZone player pos) = []
  | otherwise =
      [ Move pos targetPos
        | dc <- [-1, 1],
          dr <- [-1, 1],
          let targetPos =
                Pos (chr (ord (col pos) + dc)) (row pos + dr),
          onBoard targetPos,
          not (isUndo lastMove pos targetPos),
          validPawnTarget targetPos
      ]
  where
    -- ---------- Board helpers ----------
    cellAt :: Board -> Pos -> Cell
    cellAt b (Pos c r) =
      b !! (7 - r) !! (fromEnum c - fromEnum 'a')

    onBoard :: Pos -> Bool
    onBoard (Pos c r) =
      c >= 'a' && c <= 'd' && r >= 0 && r <= 7

    inZone :: Player -> Pos -> Bool
    inZone Bottom (Pos _ r) = r <= 3
    inZone Top (Pos _ r) = r >= 4

    sameZone :: Pos -> Bool
    sameZone p = inZone player p

    isUndo :: Maybe Move -> Pos -> Pos -> Bool
    isUndo Nothing _ _ = False
    isUndo (Just (Move s t)) from to =
      from == t && to == s

    -- ---------- Zone state ----------
    zoneCells :: [Cell]
    zoneCells = cellsInZone board player

    hasDrone :: Bool
    hasDrone = any (== Drone) zoneCells

    hasQueen :: Bool
    hasQueen = any (== Queen) zoneCells

    -- ---------- Target validation ----------
    validPawnTarget :: Pos -> Bool
    validPawnTarget p
      | not (sameZone p) =
          True -- Angriff über Kanal immer erlaubt (egal ob Empty oder Figur)
      | otherwise =
          case cellAt board p of
            Empty -> True
            Pawn -> not hasDrone -- Pawn+Pawn -> Drone
            Drone -> not hasQueen -- Pawn+Drone -> Queen
            Queen -> False -- keine Fusion, also blockiert

-- #######################################################################################################
-- ################## droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 5 Functional Points                                             ##################
-- #######################################################################################################

droneMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
droneMoves board player pos lastMove
  | cellAt board pos /= Drone = []
  | not (inZone player pos) = []
  | otherwise =
      [ Move pos p
        | (dc, dr) <- directions,
          step <- [1 .. maxSteps (dc, dr)],
          let p =
                Pos
                  (chr (ord (col pos) + dc * step))
                  (row pos + dr * step),
          onBoard p,
          pathClear pos p,
          not (isUndo lastMove pos p),
          validDroneTarget p
      ]
  where
    -- ---------------- Directions ----------------
    directions :: [(Int, Int)]
    directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

    -- ---------------- Board helpers ----------------
    cellAt :: Board -> Pos -> Cell
    cellAt b (Pos c r) =
      b !! (7 - r) !! (fromEnum c - fromEnum 'a')

    onBoard :: Pos -> Bool
    onBoard (Pos c r) =
      c >= 'a' && c <= 'd' && r >= 0 && r <= 7

    inZone :: Player -> Pos -> Bool
    inZone Top (Pos _ r) = r >= 4
    inZone Bottom (Pos _ r) = r <= 3

    sameZone :: Pos -> Bool
    sameZone p = inZone player p

    -- ---------------- Undo ----------------
    isUndo :: Maybe Move -> Pos -> Pos -> Bool
    isUndo Nothing _ _ = False
    isUndo (Just (Move s t)) from to =
      from == t && to == s

    -- ---------------- Zone state ----------------
    zoneCells :: [Cell]
    zoneCells = cellsInZone board player

    hasQueen :: Bool
    hasQueen = any (== Queen) zoneCells

    -- ---------------- Path checking ----------------
    pathClear :: Pos -> Pos -> Bool
    pathClear from to =
      let dc = signum (ord (col to) - ord (col from))
          dr = signum (row to - row from)
          steps =
            takeWhile
              (/= to)
              [ Pos
                  (chr (ord (col from) + dc * i))
                  (row from + dr * i)
                | i <- [1 .. 7]
              ]
       in all (\p -> cellAt board p == Empty) steps

    -- ---------------- Max steps (Figuren zählen) ----------------
    maxSteps :: (Int, Int) -> Int
    maxSteps (dc, dr) =
      let positions =
            takeWhile
              onBoard
              [ Pos
                  (chr (ord (col pos) + dc * i))
                  (row pos + dr * i)
                | i <- [1 .. 7]
              ]
          figs = length [() | p <- positions, cellAt board p /= Empty]
       in max 1 figs

    -- ---------------- Target validation ----------------
    validDroneTarget :: Pos -> Bool
    validDroneTarget p
      | not (sameZone p) =
          True -- Angriff über Kanal immer erlaubt
      | otherwise =
          case cellAt board p of
            Empty -> True
            Pawn -> not hasQueen -- Drone + Pawn → Queen
            Drone -> False -- Drone + Drone → Queen Falsch
            Queen -> False -- keine Fusion möglich

-- #######################################################################################################
-- ################## queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]      ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

queenMoves :: Board -> Player -> Pos -> Maybe Move -> [Move]
queenMoves board player pos lastMove
  | cellAt board pos /= Queen = []
  | not (inZone player pos) = []
  | otherwise =
      [ Move pos p
        | (dc, dr) <- directions,
          step <- [1 .. 7],
          let p =
                Pos
                  (chr (ord (col pos) + dc * step))
                  (row pos + dr * step),
          onBoard p,
          pathClear pos p,
          not (isUndo lastMove pos p),
          validQueenTarget p
      ]
  where
    -- ---------------- Directions ----------------
    directions :: [(Int, Int)]
    directions =
      [ (1, 0),
        (-1, 0),
        (0, 1),
        (0, -1),
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1)
      ]

    -- ---------------- Board helpers ----------------
    cellAt :: Board -> Pos -> Cell
    cellAt b (Pos c r) =
      b !! (7 - r) !! (fromEnum c - fromEnum 'a')

    onBoard :: Pos -> Bool
    onBoard (Pos c r) =
      c >= 'a' && c <= 'd' && r >= 0 && r <= 7

    inZone :: Player -> Pos -> Bool
    inZone Top (Pos _ r) = r >= 4
    inZone Bottom (Pos _ r) = r <= 3

    sameZone :: Pos -> Bool
    sameZone p = inZone player p

    -- ---------------- Undo ----------------
    isUndo :: Maybe Move -> Pos -> Pos -> Bool
    isUndo Nothing _ _ = False
    isUndo (Just (Move s t)) from to =
      from == t && to == s

    -- ---------------- Path checking ----------------
    pathClear :: Pos -> Pos -> Bool
    pathClear from to =
      let dc = signum (ord (col to) - ord (col from))
          dr = signum (row to - row from)
          steps =
            takeWhile
              (/= to)
              [ Pos
                  (chr (ord (col from) + dc * i))
                  (row from + dr * i)
                | i <- [1 .. 7]
              ]
       in all (\p -> cellAt board p == Empty) steps

    -- ---------------- Target validation ----------------
    validQueenTarget :: Pos -> Bool
    validQueenTarget p
      | not (sameZone p) = True -- Angriff über Kanal
      | otherwise =
          cellAt board p == Empty -- eigene Zone: nur leeres Feld

-- #######################################################################################################
-- ################## makeMove :: Board -> Move -> (Board -> Int)                       ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

makeMove :: Board -> Move -> (Board, Int)
makeMove board (Move s t) =
  let sIx = posToIndex s
      tIx = posToIndex t

      src = getCell board sIx
      dst = getCell board tIx

      sameZone = zoneOf (row s) == zoneOf (row t)

      scoreOf :: Cell -> Int
      scoreOf Pawn = 1
      scoreOf Drone = 2
      scoreOf Queen = 3
      scoreOf Empty = 0 -- optional; bleibt korrekt, falls je aufgerufen
      (finalCell, points) =
        case dst of
          Empty ->
            (src, scoreOf Empty)
          _
            | sameZone ->
                case (src, dst) of
                  (Pawn, Pawn) -> (Drone, 0)
                  (Pawn, Drone) -> (Queen, 0)
                  (Drone, Pawn) -> (Queen, 0)
                  _ -> (src, 0)
            | otherwise ->
                (src, scoreOf dst)

      board' =
        setCell
          (setCell board sIx Empty)
          tIx
          finalCell
   in (board', points)

-- -------------------------
-- Helpers (dürfen in Logic.hs stehen)
-- -------------------------

posToIndex :: Pos -> (Int, Int)
posToIndex (Pos c r) =
  (7 - r, fromEnum c - fromEnum 'a')

-- board!!0 entspricht Reihe 7 (oben)

getCell :: Board -> (Int, Int) -> Cell
getCell b (ri, ci) =
  (b !! ri) !! ci

setCell :: Board -> (Int, Int) -> Cell -> Board
setCell b (ri, ci) newCell =
  take ri b
    ++ [ take ci (b !! ri)
           ++ [newCell]
           ++ drop (ci + 1) (b !! ri)
       ]
    ++ drop (ri + 1) b

zoneOf :: Int -> Player
zoneOf r
  | r >= 4 = Top
  | otherwise = Bottom

-- #######################################################################################################
-- ################## playerWon :: Board -> Player -> Int -> Int -> Maybe Player        ##################
-- ################## - 3 Functional Points                                             ##################
-- #######################################################################################################

playerWon :: Board -> Player -> Int -> Int -> Maybe Player
playerWon board lastPlayer topScore bottomScore =
  let topEmpty = all (== Empty) (cellsInZone board Top)
      bottomEmpty = all (== Empty) (cellsInZone board Bottom)
   in if topEmpty || bottomEmpty
        then case compare topScore bottomScore of
          GT -> Just Top
          LT -> Just Bottom
          EQ -> Just lastPlayer
        else Nothing

cellsInZone :: Board -> Player -> [Cell]
cellsInZone b pl =
  let rowsToTake =
        case pl of
          Top -> [0 .. 3] -- entspricht Reihen 7..4
          Bottom -> [4 .. 7] -- entspricht Reihen 3..0
   in concatMap (b !!) rowsToTake