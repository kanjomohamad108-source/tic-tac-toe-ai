module Board where  -- do NOT CHANGE export of module
--Diese Hausaaufgabe wurde unter Verwendung von KI-Werkzeugen (insbesondere ChatGPT und Gemini) bearbeitet.

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars
import Data.Char(isDigit)
import Text.Read(readMaybe)

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Given data types may NOT be changed            ################
-- #############################################################################

data Player = Top | Bottom deriving (Show, Read)
data Cell = Empty | Queen | Drone | Pawn deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Top Top = True
  (==) Bottom Bottom = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) Pawn Pawn = True
  (==) Drone Drone = True
  (==) Queen Queen = True
  (==) _ _ = False
  
startingFEN :: String
startingFEN = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"

startingBoard :: [[Cell]]
startingBoard = [
  [Queen, Queen, Drone, Empty],
  [Queen, Drone, Pawn,  Empty],
  [Drone, Pawn,  Pawn,  Empty],
  [Empty, Empty, Empty, Empty],
  [Empty, Empty, Empty, Empty],
  [Empty, Pawn,  Pawn,  Drone],
  [Empty, Pawn,  Drone, Queen],
  [Empty, Drone, Queen, Queen]
  ]

buildPos :: String -> Pos 
buildPos (c:rStr) = Pos c (read rStr) 
buildPos _ = error "Invalid position format"

-- ##############################################################################
-- ################## IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

validateFEN :: String -> Bool
validateFEN fen
  | fen == "///////" = True   -- leeres Brett explizit erlauben
  | otherwise =
      let rows = split '/' fen
      in  length rows == 8
          && not (null fen)
          && head fen /= '/'
          && last fen /= '/'
          && all validRow rows
  where
    -- Eine Reihe ist gültig, wenn:
    -- 1) sie leer ist (repräsentiert exakt 4 leere Felder)
    -- ODER
    -- 2) sie exakt 4 Felder ergibt UND mindestens eine Figur enthält
    validRow :: String -> Bool
    validRow "" = True
    validRow row =
      case countFields row of
        Just (fields, hasFigure) -> fields == 4 && hasFigure
        Nothing                 -> False

    -- zählt Felder; Ziffern zählen einzeln
    -- merkt sich zusätzlich, ob mindestens eine Figur vorkommt
    countFields :: String -> Maybe (Int, Bool)
    countFields [] = Just (0, False)
    countFields (c:cs)
      | c `elem` "pdq" =
          fmap (\(n, _) -> (n + 1, True)) (countFields cs)

      | isDigit c =
          case readMaybe [c] of
            Just n | n >= 1 && n <= 4 ->
              fmap (\(k, f) -> (k + n, f)) (countFields cs)
            _ -> Nothing               -- deckt ungültige Ziffern ab

      | otherwise = Nothing             -- deckt sonstige Zeichen ab

    -- eigener Split ohne Imports
    split :: Char -> String -> [String]
    split _ [] = [""]
    split sep (c:cs)
      | c == sep  = "" : rest
      | otherwise = (c : head rest) : tail rest
      where
        rest = split sep cs

-- ##############################################################################
-- ################## IMPLEMENT buildBoard :: String -> Board ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildBoard :: String -> Board
buildBoard fen = map buildRow (split '/' fen)
  where
    buildRow :: String -> [Cell]
    buildRow row = take 4 (expand row ++ repeat Empty)

    expand :: String -> [Cell]
    expand [] = []
    expand (c:cs)
      | c == 'p' = Pawn  : expand cs
      | c == 'd' = Drone : expand cs
      | c == 'q' = Queen : expand cs
      | isDigit c = replicate (read [c]) Empty ++ expand cs
      | otherwise = []

    split :: Char -> String -> [String]
    split _ [] = [""]
    split sep (c:cs)
      | c == sep  = "" : rest
      | otherwise = (c : head rest) : tail rest
      where
        rest = split sep cs

-- ##############################################################################
-- ################## IMPLEMENT buildFEN :: Board -> String   ###################
-- ################## - 1 Functional Point                   ####################
-- ##############################################################################

buildFEN :: Board -> String
buildFEN board = concatRows (map encodeRow board)
  where
    encodeRow :: [Cell] -> String
    encodeRow row
      | all (== Empty) row = ""
      | otherwise = encodeCells row

    encodeCells :: [Cell] -> String
    encodeCells [] = ""
    encodeCells cells =
      let (empties, rest) = span (== Empty) cells
          prefix = if length empties == 0 then "" else show (length empties)
      in case rest of
           [] ->
             prefix
           (c:cs) ->
             prefix ++ [cellChar c] ++ encodeCells cs

    cellChar :: Cell -> Char
    cellChar Pawn  = 'p'
    cellChar Drone = 'd'
    cellChar Queen = 'q'
    cellChar Empty = error "Empty should not be encoded directly"

    concatRows :: [String] -> String
    concatRows []     = ""
    concatRows [r]    = r
    concatRows (r:rs) = r ++ "/" ++ concatRows rs