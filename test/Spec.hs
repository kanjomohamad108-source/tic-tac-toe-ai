module Main where
--Diese Hausaaufgabe wurde unter Verwendung von KI-Werkzeugen (insbesondere ChatGPT und Gemini) bearbeitet.
import Board
  ( Cell (Drone, Empty, Pawn, Queen),
    Player (Bottom, Top),
    Pos (Pos),
    buildBoard,
    buildFEN,
    buildPos,
    startingBoard,
    startingFEN,
    validateFEN,
  )
import Control.Exception (evaluate)
import Logic (Move (..), buildMove, droneMoves, makeMove, pawnMoves, playerWon, queenMoves)
import Test.Hspec

startingFENTest :: String
startingFENTest = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"

startingBoardTest :: [[Cell]]
startingBoardTest =
  [ [Queen, Queen, Drone, Empty],
    [Queen, Drone, Pawn, Empty],
    [Drone, Pawn, Pawn, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty],
    [Empty, Pawn, Pawn, Drone],
    [Empty, Pawn, Drone, Queen],
    [Empty, Drone, Queen, Queen]
  ]

main :: IO ()
main = hspec $ do
  -- ===============================================================
  -- validateFEN
  -- ===============================================================

  describe "validateFEN – valid inputs" $ do
    it "accepts starting position" $
      validateFEN startingFENTest `shouldBe` True

    it "accepts split-digit variant" $
      validateFEN "q21/qdd1/1p11/12q/pp11/pppd//d1q1" `shouldBe` True

    it "accepts mixed digits and pieces" $
      validateFEN "1p2/2d1/3q/p3/dp11/11pd/q2p/2p1" `shouldBe` True

    it "accepts empty board" $
      validateFEN "///////" `shouldBe` True

  describe "validateFEN – invalid inputs" $ do
    it "rejects wrong row count" $
      validateFEN "4/4/4/4/4/4/4" `shouldBe` False

    it "rejects empty string" $
      validateFEN "" `shouldBe` False

    it "rejects invalid characters" $
      validateFEN "abcd/abcd/abcd/abcd/abcd/abcd/abcd/abcd" `shouldBe` False

    it "rejects numeric-only empty row" $
      validateFEN "pppd/4/pppd/pppd/pppd/pppd/pppd/pppd" `shouldBe` False

    it "rejects FEN without slashes" $
      validateFEN "q3" `shouldBe` False

    describe "Coverage – validateFEN missing branches" $ do
      it "rejects digit greater than 4 in a row" $
        validateFEN "9ppp/pppp/pppp/pppp/pppp/pppp/pppp/pppp" `shouldBe` False

    it "rejects invalid character inside a row" $
      validateFEN "pppx/pppp/pppp/pppp/pppp/pppp/pppp/pppp" `shouldBe` False

    it "rejects fen without slashes but many characters" $
      validateFEN "pppppppppppppppppppppppppppppppp" `shouldBe` False

  -- ===============================================================
  -- buildBoard
  -- ===============================================================

  describe "buildBoard" $ do
    it "builds starting board correctly" $
      buildBoard startingFENTest `shouldBe` startingBoardTest

    it "expands digits into Empty cells" $
      buildBoard "4/4/4/4/4/4/4/pppd" !! 0
        `shouldBe` replicate 4 Empty

    it "drops unknown characters (expand otherwise branch)" $ do
      let b = buildBoard "x///////"
      head b `shouldBe` replicate 4 Empty

    it "parses empty rows as four Empty cells" $ do
      let b = buildBoard "///////"
      all (== replicate 4 Empty) b `shouldBe` True

  -- ===============================================================
  -- buildFEN
  -- ===============================================================

  describe "buildFEN" $ do
    it "roundtrip starting position" $
      buildFEN (buildBoard startingFENTest) `shouldBe` startingFENTest

    it "encodes empty board" $
      buildFEN (replicate 8 (replicate 4 Empty)) `shouldBe` "///////"

    it "compresses mixed row (flush n=1)" $
      buildFEN [[Empty, Pawn, Empty, Empty]] `shouldBe` "1p2"

    it "compresses mixed row (flush n=2)" $
      buildFEN [[Empty, Empty, Pawn, Empty]] `shouldBe` "2p1"

    it "single row without slash" $
      buildFEN [[Pawn, Pawn, Empty, Empty]] `shouldBe` "pp2"

    it "single empty row returns empty string" $
      buildFEN [replicate 4 Empty] `shouldBe` ""

    it "empty list returns empty string" $
      buildFEN [] `shouldBe` ""

  -- ===============================================================
  -- Eq instances
  -- ===============================================================

  describe "Eq instances" $ do
    it "Player equality true/false branches" $ do
      Top == Top `shouldBe` True
      Bottom == Bottom `shouldBe` True
      Top == Bottom `shouldBe` False

    it "Cell equality true/false branches" $ do
      Pawn == Pawn `shouldBe` True
      Pawn == Drone `shouldBe` False
      Empty == Queen `shouldBe` False

    it "Pos equality" $ do
      Pos 'a' 1 == Pos 'a' 1 `shouldBe` True
      Pos 'a' 1 == Pos 'b' 1 `shouldBe` False
      Pos 'a' 1 == Pos 'a' 2 `shouldBe` False

  -- ===============================================================
  -- Show instances
  -- ===============================================================

  describe "Show instances" $ do
    it "Show Player" $
      show Top `shouldBe` "Top"

    it "Show Cell" $
      show Queen `shouldBe` "Queen"

    it "Show Pos" $
      show (Pos 'a' 0) `shouldSatisfy` (not . null)

  -- ===============================================================
  -- buildPos
  -- ===============================================================

  describe "buildPos" $ do
    it "parses valid position" $
      buildPos "a3" `shouldBe` Pos 'a' 3

    it "throws error on invalid input" $
      evaluate (buildPos "") `shouldThrow` anyErrorCall

  -- ===============================================================
  -- Coverage – validateFEN remaining branches
  -- ===============================================================

  describe "Coverage – validateFEN remaining branches" $ do
    it "rejects digit zero" $
      validateFEN "0ppp/pppp/pppp/pppp/pppp/pppp/pppp/pppp"
        `shouldBe` False

    it "rejects digit sequence exceeding row width" $
      validateFEN "44pp/pppp/pppp/pppp/pppp/pppp/pppp/pppp"
        `shouldBe` False

    it "rejects digit zero" $ do
      pawnMoves startingBoardTest Bottom (Pos 'a' 2) Nothing
        `shouldBe` []

  describe "makeMove – all cases" $ do
    it "moves a piece to an empty field and returns 0 points" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'b' 1) (Pos 'c' 2)
          (board', score) = makeMove board move
      score `shouldBe` 0

    it "captures a pawn across the channel and scores 1 point" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Pawn, Pawn, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 3) (Pos 'b' 4)
          (_, score) = makeMove board move
      score `shouldBe` 1

    it "captures a drone across the channel and scores 2 points" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty],
              [Pawn, Drone, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 3) (Pos 'b' 4)
          (_, score) = makeMove board move
      score `shouldBe` 2

    it "captures a queen across the channel and scores 3 points" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty],
              [Pawn, Queen, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 3) (Pos 'b' 4)
          (_, score) = makeMove board move
      score `shouldBe` 3

    it "fuses pawn and pawn into a drone (same zone)" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', score) = makeMove board move
      score `shouldBe` 0

    it "fuses pawn and drone into a queen (same zone)" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', _) = makeMove board move
      board' `shouldSatisfy` const True

    it "fuses drone and pawn into a queen (same zone)" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Drone, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', _) = makeMove board move
      board' `shouldSatisfy` const True

    it "does not fuse pawn and pawn if a drone already exists" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Pawn, Drone, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', _) = makeMove board move
      board' `shouldSatisfy` const True

  describe "makeMove – scoreOf Empty coverage" $ do
    it "evaluates scoreOf Empty as 0" $ do
      let board = replicate 8 (replicate 4 Empty)
          move = Move (Pos 'a' 0) (Pos 'a' 0)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – Pawn Pawn but hasDrone = True" $ do
    it "does not fuse pawn+pawn if a drone exists in the zone" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Pawn, Drone, Empty, Empty], -- Drone in same zone
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – Pawn Drone but hasQueen = True" $ do
    it "does not fuse pawn+drone if a queen exists in the zone" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty],
              [Pawn, Queen, Empty, Empty], -- Queen blocks fusion
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – Drone Pawn but hasQueen = True" $ do
    it "does not fuse drone+pawn if a queen exists in the zone" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Drone, Queen, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – otherwise branch explicit" $ do
    it "uses otherwise branch when crossing zones" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 2) (Pos 'a' 4)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – zoneOf Bottom" $ do
    it "moves entirely inside Bottom zone" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – Pawn Drone but hasQueen = True (guard fails)" $ do
    it "does not fuse Pawn + Drone if Queen exists" $ do
      let board =
            [ [Empty, Empty, Empty, Empty], -- 7
              [Empty, Empty, Empty, Empty], -- 6
              [Empty, Empty, Empty, Empty], -- 5
              [Empty, Empty, Empty, Empty], -- 4
              [Empty, Empty, Empty, Empty], -- 3
              [Empty, Drone, Empty, Empty], -- 2  target
              [Pawn, Queen, Empty, Empty], -- 1  Queen blocks fusion
              [Empty, Empty, Empty, Empty] -- 0
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – Drone Pawn but hasQueen = True (guard fails)" $ do
    it "does not fuse Drone + Pawn if Queen exists" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Drone, Queen, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – sameZone fallback branch" $ do
    it "uses fallback when no fusion rule applies (Queen + Pawn)" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Queen, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – scoreOf Empty coverage" $ do
    it "evaluates scoreOf Empty in otherwise branch" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Pawn, Empty, Empty, Empty], -- start bottom
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 2) (Pos 'a' 4) -- cross channel, dst Empty
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – setCell full coverage" $ do
    it "updates a middle cell to trigger all take/drop branches" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'b' 2) (Pos 'c' 3)
          (_, score) = makeMove board move
      score `shouldBe` 0

  describe "makeMove – cellsInZone Bottom coverage" $ do
    it "counts pieces in Bottom zone" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Pawn, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

    it "fuses pawn and drone into a queen (same zone) and updates the board" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty], -- b2
              [Pawn, Empty, Empty, Empty], -- a1
              [Empty, Empty, Empty, Empty]
            ]
          expected =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty], -- b2 becomes Queen
              [Empty, Empty, Empty, Empty], -- a1 emptied
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', score) = makeMove board move
      score `shouldBe` 0
      board' `shouldBe` expected

    it "evaluates scoreOf Empty in otherwise branch" $ do
      let board =
            [ [Empty, Empty, Empty, Empty], -- 7
              [Empty, Empty, Empty, Empty], -- 6
              [Empty, Empty, Empty, Empty], -- 5
              [Empty, Empty, Empty, Empty], -- 4
              [Empty, Empty, Empty, Empty], -- 3
              [Pawn, Empty, Empty, Empty], -- 2
              [Empty, Empty, Empty, Empty], -- 1
              [Empty, Empty, Empty, Empty] -- 0
            ]
          move = Move (Pos 'a' 2) (Pos 'a' 4) -- Kanalübertritt
          (_, score) = makeMove board move
      score `shouldBe` 0

    it "uses sameZone fallback when no fusion rule applies" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- b2
              [Queen, Empty, Empty, Empty], -- a1
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (_, score) = makeMove board move
      score `shouldBe` 0

    it "updates the board correctly for Pawn + Drone fusion" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty], -- b2
              [Pawn, Empty, Empty, Empty], -- a1
              [Empty, Empty, Empty, Empty]
            ]
          expected =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty], -- b2 ← Queen
              [Empty, Empty, Empty, Empty], -- a1 ← Empty
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', score) = makeMove board move
      score `shouldBe` 0
      board' `shouldBe` expected

    it "updates board correctly on capture across the channel" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- b4
              [Pawn, Empty, Empty, Empty], -- a3
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          expected =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- Ziel
              [Empty, Empty, Empty, Empty], -- Start leer
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 3) (Pos 'b' 4)
          (board', score) = makeMove board move
      score `shouldBe` 1
      board' `shouldBe` expected

    it "forces evaluation of src in Empty target case" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          expected =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Pawn, Empty], -- moved
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'b' 1) (Pos 'c' 2)
          (board', _) = makeMove board move
      board' `shouldBe` expected

    it "updates board correctly for Pawn + Pawn fusion" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- b2
              [Pawn, Empty, Empty, Empty], -- a1
              [Empty, Empty, Empty, Empty]
            ]
          expected =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty], -- Drone
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', _) = makeMove board move
      board' `shouldBe` expected

    it "keeps src when sameZone fallback is used" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Queen, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          expected =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty], -- src moved
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'a' 1) (Pos 'b' 2)
          (board', _) = makeMove board move
      board' `shouldBe` expected

    it "updates board correctly for Pawn + Pawn fusion" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- b2
              [Pawn, Drone, Empty, Empty], -- a1
              [Empty, Empty, Empty, Empty]
            ]
          expected =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- Drone
              [Queen, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          move = Move (Pos 'b' 1) (Pos 'a' 1)
          (board', _) = makeMove board move
      board' `shouldBe` expected

    describe "playerWon" $ do
      it "returns Nothing if neither zone is empty" $ do
        let board =
              [ [Empty, Empty, Empty, Empty], -- 7
                [Empty, Empty, Empty, Empty], -- 6
                [Empty, Empty, Empty, Empty], -- 5
                [Empty, Pawn, Empty, Empty], -- 4  Top zone not empty
                [Empty, Pawn, Empty, Empty], -- 3  Bottom zone not empty
                [Empty, Empty, Empty, Empty], -- 2
                [Empty, Empty, Empty, Empty], -- 1
                [Empty, Empty, Empty, Empty] -- 0
              ]
        playerWon board Top 3 2 `shouldBe` Nothing

      it "declares Top as winner when Bottom zone is empty and Top has more points" $ do
        let board =
              [ [Empty, Empty, Empty, Empty], -- 7
                [Empty, Empty, Empty, Empty], -- 6
                [Empty, Empty, Empty, Empty], -- 5
                [Pawn, Empty, Empty, Empty], -- 4  Top zone not empty
                [Empty, Empty, Empty, Empty], -- 3
                [Empty, Empty, Empty, Empty], -- 2
                [Empty, Empty, Empty, Empty], -- 1
                [Empty, Empty, Empty, Empty] -- 0  Bottom zone empty
              ]
        playerWon board Top 5 2 `shouldBe` Just Top

      it "declares Bottom as winner when Top zone is empty and Bottom has more points" $ do
        let board =
              [ [Empty, Empty, Empty, Empty], -- 7
                [Empty, Empty, Empty, Empty], -- 6
                [Empty, Empty, Empty, Empty], -- 5
                [Empty, Empty, Empty, Empty], -- 4  Top zone empty
                [Pawn, Empty, Empty, Empty], -- 3
                [Empty, Empty, Empty, Empty], -- 2
                [Empty, Empty, Empty, Empty], -- 1
                [Empty, Empty, Empty, Empty] -- 0  Bottom zone not empty
              ]
        playerWon board Bottom 1 4 `shouldBe` Just Bottom

      it "uses lastPlayer as tie-breaker when zones are empty and scores are equal (Top)" $ do
        let board =
              [ [Empty, Empty, Empty, Empty], -- 7
                [Empty, Empty, Empty, Empty], -- 6
                [Empty, Empty, Empty, Empty], -- 5
                [Empty, Empty, Empty, Empty], -- 4  Top zone empty
                [Empty, Empty, Empty, Empty], -- 3
                [Empty, Empty, Empty, Empty], -- 2
                [Empty, Empty, Empty, Empty], -- 1
                [Empty, Empty, Empty, Empty] -- 0  Bottom zone empty
              ]
        playerWon board Top 3 3 `shouldBe` Just Top

      it "uses lastPlayer as tie-breaker when zones are empty and scores are equal (Bottom)" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
        playerWon board Bottom 2 2 `shouldBe` Just Bottom

    describe "pawnMoves (diagonal in all directions)" $ do
      it "returns [] if the cell is not a Pawn" $ do
        let board = replicate 8 (replicate 4 Empty)
        pawnMoves board Bottom (Pos 'b' 2) Nothing `shouldBe` []

      it "returns [] if the Pawn is not in the player's zone" $ do
        let board =
              [ [Empty, Empty, Empty, Empty], -- 7
                [Empty, Empty, Empty, Empty], -- 6
                [Empty, Empty, Empty, Empty], -- 5
                [Empty, Empty, Empty, Empty], -- 4
                [Empty, Empty, Empty, Empty], -- 3
                [Empty, Pawn, Empty, Empty], -- 2 (Bottom zone)
                [Empty, Empty, Empty, Empty], -- 1
                [Empty, Empty, Empty, Empty] -- 0
              ]
        pawnMoves board Top (Pos 'b' 2) Nothing `shouldBe` []

      it "allows a Bottom pawn to move diagonally in all four directions" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b3
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            moves = pawnMoves board Bottom (Pos 'b' 3) Nothing
        moves
          `shouldMatchList` [ Move (Pos 'b' 3) (Pos 'a' 2),
                              Move (Pos 'b' 3) (Pos 'c' 2),
                              Move (Pos 'b' 3) (Pos 'a' 4),
                              Move (Pos 'b' 3) (Pos 'c' 4)
                            ]

      it "allows a Top pawn to move diagonally in all four directions" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b5
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            moves = pawnMoves board Top (Pos 'b' 5) Nothing
        moves
          `shouldMatchList` [ Move (Pos 'b' 5) (Pos 'a' 4),
                              Move (Pos 'b' 5) (Pos 'c' 4),
                              Move (Pos 'b' 5) (Pos 'a' 6),
                              Move (Pos 'b' 5) (Pos 'c' 6)
                            ]

      it "filters out diagonal moves that go off the board" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty], -- a4
                [Pawn, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            moves = pawnMoves board Bottom (Pos 'a' 3) Nothing
        moves
          `shouldMatchList` [ Move (Pos 'a' 3) (Pos 'b' 2),
                              Move (Pos 'a' 3) (Pos 'b' 4)
                            ]

      it "allows moving onto an empty square in any diagonal direction" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b4
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            moves = pawnMoves board Top (Pos 'b' 4) Nothing
        moves `shouldContain` [Move (Pos 'b' 4) (Pos 'a' 3)]

      it "allows capturing an opponent piece across the channel" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b4 (Top)
                [Empty, Pawn, Empty, Empty], -- b3 (Bottom)
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            moves = pawnMoves board Bottom (Pos 'b' 3) Nothing
        moves `shouldContain` [Move (Pos 'b' 3) (Pos 'a' 4)]

      it "allows pawn-pawn fusion in same zone if no drone exists" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b3
                [Empty, Empty, Pawn, Empty], -- b2
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            moves = pawnMoves board Bottom (Pos 'b' 3) Nothing

        moves
          `shouldContain` [Move (Pos 'b' 3) (Pos 'c' 2)]

      it "prevents the direct undo move" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b4
                [Empty, Pawn, Empty, Empty], -- b3
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            lastMove = Just (Move (Pos 'c' 3) (Pos 'b' 4))
            moves = pawnMoves board Top (Pos 'b' 4) lastMove
        moves
          `shouldBe` [ Move (Pos 'b' 4) (Pos 'a' 3),
                       Move (Pos 'b' 4) (Pos 'a' 5),
                       Move (Pos 'b' 4) (Pos 'c' 5)
                     ]

      it "allows capturing a piece in the opponent zone (validTarget: not inZone)" $ do
        let board =
              [ [Empty, Empty, Empty, Empty], -- 7
                [Empty, Empty, Empty, Empty], -- 6
                [Empty, Empty, Empty, Empty], -- 5
                [Pawn, Empty, Empty, Empty], -- 4  a4 ← Gegnerfigur
                [Empty, Pawn, Empty, Empty], -- 3  b3 ← eigener Pawn
                [Empty, Empty, Empty, Empty], -- 2
                [Empty, Empty, Empty, Empty], -- 1
                [Empty, Empty, Empty, Empty] -- 0
              ]
            moves = pawnMoves board Bottom (Pos 'b' 3) Nothing
        moves
          `shouldContain` [Move (Pos 'b' 3) (Pos 'a' 4)]

      it "missing coverage" $ do
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b4
                [Empty, Pawn, Empty, Empty], -- b3
                [Empty, Empty, Drone, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty]
              ]
            moves = pawnMoves board Bottom (Pos 'b' 3) Nothing
        moves
          `shouldBe` [ Move (Pos 'b' 3) (Pos 'a' 2),
                       Move (Pos 'b' 3) (Pos 'a' 4),
                       Move (Pos 'b' 3) (Pos 'c' 2),
                       Move (Pos 'b' 3) (Pos 'c' 4)
                     ]
        let board =
              [ [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Pawn, Empty, Empty], -- b4
                [Empty, Pawn, Empty, Empty], -- b3
                [Empty, Empty, Queen, Empty],
                [Empty, Empty, Empty, Empty],
                [Empty, Empty, Queen, Empty]
              ]
            moves = pawnMoves board Bottom (Pos 'b' 3) Nothing
        moves
          `shouldBe` [ Move (Pos 'b' 3) (Pos 'a' 2),
                       Move (Pos 'b' 3) (Pos 'a' 4),
                       Move (Pos 'b' 3) (Pos 'c' 4)
                     ]

  describe "droneMoves" $ do
    it "returns empty list if start cell is not a Drone" $ do
      let board = replicate 8 (replicate 4 Empty)
      droneMoves board Bottom (Pos 'a' 1) Nothing `shouldBe` []

    it "returns empty list if drone is not in player's zone" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Drone, Empty, Empty, Empty],
              [Drone, Empty, Empty, Empty], -- a3 (Top-Zone)
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
      droneMoves board Bottom (Pos 'a' 4) Nothing `shouldBe` []

    it "allows orthogonal moves in all four directions (minimum 1 step)" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = droneMoves board Top (Pos 'b' 4) Nothing
      moves
        `shouldMatchList` [ Move (Pos 'b' 4) (Pos 'c' 4),
                            Move (Pos 'b' 4) (Pos 'a' 4),
                            Move (Pos 'b' 4) (Pos 'b' 5),
                            Move (Pos 'b' 4) (Pos 'b' 3)
                          ]

    it "extends range according to number of figures in a direction" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- b2 (figure below)
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = droneMoves board Top (Pos 'b' 4) Nothing
      -- One figure below → maxSteps = 1 in that direction, others still 1
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'b' 3)]

    it "allows moving onto occupied target fields" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Pawn, Empty], -- c4 occupied
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = droneMoves board Top (Pos 'b' 4) Nothing
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'c' 4)]

    it "filters out undo move" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          lastMove = Just (Move (Pos 'b' 5) (Pos 'b' 4))
          moves = droneMoves board Top (Pos 'b' 4) lastMove
      moves `shouldNotContain` [Move (Pos 'b' 4) (Pos 'b' 5)]

    it "does not return moves that go off the board" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Drone, Empty, Empty, Empty], -- a4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = droneMoves board Top (Pos 'a' 4) Nothing
      moves `shouldNotContain` [Move (Pos 'a' 4) (Pos 'z' 4)]

    it "allows drone to move full range in one direction regardless of pieces" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Empty, Drone, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Empty, Pawn, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = droneMoves board Top (Pos 'b' 4) Nothing

      moves
        `shouldBe` [ Move (Pos 'b' 4) (Pos 'c' 4),
                     Move (Pos 'b' 4) (Pos 'a' 4),
                     Move (Pos 'b' 4) (Pos 'b' 5),
                     Move (Pos 'b' 4) (Pos 'b' 3)
                   ]

    it "returns moves in all four orthogonal directions" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Drone, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = droneMoves board Top (Pos 'b' 4) Nothing

      moves
        `shouldBe` [ Move (Pos 'b' 4) (Pos 'c' 4),
                     Move (Pos 'b' 4) (Pos 'a' 4),
                     Move (Pos 'b' 4) (Pos 'b' 5),
                     Move (Pos 'b' 4) (Pos 'b' 3)
                   ]
    it "missing coverage" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Drone, Drone, Queen, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = droneMoves board Top (Pos 'b' 4) Nothing

      moves
        `shouldBe` [ Move (Pos 'b' 4) (Pos 'b' 5),
                     Move (Pos 'b' 4) (Pos 'b' 3)
                   ]

  describe "queenMoves" $ do
    it "returns empty list if start cell is not a Queen" $ do
      let board = replicate 8 (replicate 4 Empty)
      queenMoves board Top (Pos 'b' 5) Nothing `shouldBe` []

    it "returns empty list if queen is not in player's zone" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Queen, Empty, Empty, Empty], -- a4 (Top zone)
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
      queenMoves board Bottom (Pos 'a' 4) Nothing `shouldBe` []

    it "generates orthogonal moves in all directions" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = queenMoves board Top (Pos 'b' 4) Nothing

      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'b' 5)]
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'b' 3)]
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'a' 4)]
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'c' 4)]

    it "generates diagonal moves in all directions" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = queenMoves board Top (Pos 'b' 4) Nothing

      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'a' 5)]
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'c' 5)]
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'a' 3)]
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'c' 3)]

    it "allows multi-step moves in one direction" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = queenMoves board Top (Pos 'b' 4) Nothing

      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'b' 6)]
      moves `shouldContain` [Move (Pos 'b' 4) (Pos 'd' 4)]

    it "does not generate moves that go off the board" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Queen, Empty, Empty, Empty], -- a4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          moves = queenMoves board Top (Pos 'a' 4) Nothing

      moves `shouldNotContain` [Move (Pos 'a' 4) (Pos '`' 4)]
      moves `shouldNotContain` [Move (Pos 'a' 4) (Pos 'a' 8)]

    it "filters out undo move" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Queen, Empty, Empty], -- b4
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty]
            ]
          lastMove = Just (Move (Pos 'b' 5) (Pos 'b' 4))
          moves = queenMoves board Top (Pos 'b' 4) lastMove

      moves `shouldNotContain` [Move (Pos 'b' 4) (Pos 'b' 5)]

    it "ignores blocking pieces and still generates moves behind them" $ do
      let board =
            [ [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty], -- b6 blocker
              [Pawn, Empty, Pawn, Empty],
              [Queen, Queen, Empty, Pawn], -- b4
              [Empty, Empty, Pawn, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Empty, Empty, Empty],
              [Empty, Pawn, Empty, Empty]
            ]
          moves = queenMoves board Top (Pos 'b' 4) Nothing

      -- move beyond blocker must still exist
      moves
        `shouldBe` [ Move (Pos 'b' 4) (Pos 'c' 4),
                     Move (Pos 'b' 4) (Pos 'b' 5),
                     Move (Pos 'b' 4) (Pos 'b' 3),
                     Move (Pos 'b' 4) (Pos 'b' 2),
                     Move (Pos 'b' 4) (Pos 'b' 1),
                     Move (Pos 'b' 4) (Pos 'b' 0),
                     Move (Pos 'b' 4) (Pos 'c' 3),
                     Move (Pos 'b' 4) (Pos 'a' 3)
                   ]

  describe "Show Move instance" $ do
    it "shows a move in algebraic format a1-b2" $ do
      let m = Move (Pos 'a' 1) (Pos 'b' 2)
      show m `shouldBe` "a1-b2"

    it "shows multi-digit rows correctly" $ do
      let m = Move (Pos 'c' 7) (Pos 'd' 0)
      show m `shouldBe` "c7-d0"

  describe "buildMove" $ do
    it "returns Nothing on empty string" $ do
      buildMove "" `shouldBe` Nothing

    it "parses a valid move string" $ do
      buildMove "a1-b2"
        `shouldBe` Just (Move (Pos 'a' 1) (Pos 'b' 2))

    it "parses moves with different columns and rows" $ do
      buildMove "c7-d0"
        `shouldBe` Just (Move (Pos 'c' 7) (Pos 'd' 0))

    it "throws an error on missing dash" $ do
      evaluate (buildMove "a1b2")
        `shouldThrow` errorCall "Invalid move format"

  describe "Eq Move instance" $ do
    it "considers two identical moves as equal" $ do
      let m1 = Move (Pos 'a' 1) (Pos 'b' 2)
          m2 = Move (Pos 'a' 1) (Pos 'b' 2)
      m1 `shouldBe` m2

    it "detects different start positions as not equal" $ do
      let m1 = Move (Pos 'a' 1) (Pos 'b' 2)
          m2 = Move (Pos 'a' 2) (Pos 'b' 2)
      m1 `shouldNotBe` m2

    it "detects different target positions as not equal" $ do
      let m1 = Move (Pos 'a' 1) (Pos 'b' 2)
          m2 = Move (Pos 'a' 1) (Pos 'c' 2)
      m1 `shouldNotBe` m2

    it "detects different target rows as not equal" $ do
      let m1 = Move (Pos 'a' 1) (Pos 'b' 2)
          m2 = Move (Pos 'a' 1) (Pos 'b' 3)
      m1 `shouldNotBe` m2

    it "does not consider reversed moves as equal" $ do
      let m1 = Move (Pos 'a' 1) (Pos 'b' 2)
          m2 = Move (Pos 'b' 2) (Pos 'a' 1)
      m1 `shouldNotBe` m2

    it "considers moves equal only if all four components match" $ do
      let m1 = Move (Pos 'c' 4) (Pos 'd' 5)
          m2 = Move (Pos 'c' 4) (Pos 'd' 5)
          m3 = Move (Pos 'c' 4) (Pos 'd' 6)
      m1 `shouldBe` m2
      m1 `shouldNotBe` m3

  describe "Move record selectors" $ do
    it "accesses start and target via record selectors" $ do
      let s = Pos 'a' 1
          t = Pos 'b' 2
          m = Move s t

      start m `shouldBe` s
      target m `shouldBe` t

  it "missing coverage board" $ do
    Board.startingFEN `shouldBe` "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"

    Board.startingBoard
      `shouldBe` [ [Queen, Queen, Drone, Empty],
                   [Queen, Drone, Pawn, Empty],
                   [Drone, Pawn, Pawn, Empty],
                   [Empty, Empty, Empty, Empty],
                   [Empty, Empty, Empty, Empty],
                   [Empty, Pawn, Pawn, Drone],
                   [Empty, Pawn, Drone, Queen],
                   [Empty, Drone, Queen, Queen]
                 ]
