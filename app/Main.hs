module Main (main) where

import System.Environment

import Logic
import Board

main :: IO ()
main = do
    args <- getArgs
    case args of
      ("pawnMoves":fen:pStr:posStr:lastStr:_) -> 
        print (pawnMoves (buildBoard fen) (read pStr) (buildPos posStr) (buildMove lastStr))
      ("droneMoves":fen:pStr:posStr:lastStr:_) -> 
        print (droneMoves (buildBoard fen) (read pStr) (buildPos posStr) (buildMove lastStr))
      ("queenMoves":fen:pStr:posStr:lastStr:_) -> 
        print (queenMoves (buildBoard fen) (read pStr) (buildPos posStr) (buildMove lastStr))
      ("makeMove":fen:mvStr:_) -> do
        case buildMove mvStr of
          Nothing -> putStrLn "Invalid move"
          Just mv -> do
            let (b', score) = makeMove (buildBoard fen) mv
            putStrLn (buildFEN b' ++ " " ++ show score)
      ("playerWon":fen:pStr:topPts:bottomPts:_) -> 
        print (playerWon (buildBoard fen) (read pStr) (read topPts) (read bottomPts))
      _ -> putStrLn "Unsupported or malformed arguments"
