-- ##################################################################################
-- ###### VALIDATION TESTS                                                 ##########
-- ###### (DO NOT CHANGE ANYTHING)                                         ##########
-- ###### Note: execute tests using "stack test martian-chess:validate"    ##########
-- ##################################################################################

import Test.Hspec
import Board
import Logic

main :: IO ()
main = hspec $ do
  testValidateFEN
  testBuildBoard
  testBuildFEN
  testPawnMoves
  testDroneMoves
  testQueenMoves
  testMakeMove
  testPlayerWon

testValidateFEN :: Spec
testValidateFEN = describe "IF Validate-Module-Board: validateFEN" $ do
  it "starting FEN is valid" $ do
    validateFEN startingFEN `shouldBe` True

testBuildBoard :: Spec
testBuildBoard = describe "IF Validate-Module-Board: buildBoard" $ do
  it "builds starting board from starting FEN" $ do
    buildBoard startingFEN `shouldBe` startingBoard

testBuildFEN :: Spec
testBuildFEN = describe "IF Validate-Module-Board: buildFEN" $ do
  it "builds starting FEN from starting board" $ do
    buildFEN startingBoard `shouldBe` startingFEN

testPawnMoves:: Spec
testPawnMoves = describe "IF Validate-Module-Logic: pawnMoves ..." $ do
  it "wrong position" $ do
    pawnMoves startingBoard Bottom (Pos 'a' 7) (Just (Move (Pos 'a' 0) (Pos 'a' 0))) `shouldBe` []

testDroneMoves :: Spec
testDroneMoves = describe "IF Validate-Module-Logic: droneMoves" $ do
  it "wrong position" $ do
    droneMoves startingBoard Top (Pos 'a' 0) (Just (Move (Pos 'a' 0) (Pos 'a' 0))) `shouldBe` []

testQueenMoves :: Spec
testQueenMoves = describe "IF Validate-Module-Logic: queenMoves" $ do
  it "wrong position" $ do
    queenMoves startingBoard Bottom (Pos 'a' 7) (Just (Move (Pos 'a' 0) (Pos 'a' 0))) `shouldBe` []

testMakeMove :: Spec
testMakeMove = describe "IF Validate-Module-Logic: makeMove" $ do
  it "nothing happens" $ do
    makeMove startingBoard (Move (Pos 'a' 0) (Pos 'a' 0)) `shouldBe` (startingBoard, 0)

testPlayerWon :: Spec
testPlayerWon = describe "IF Validate-Module-Logic: playerWon" $ do
  it "no player has won yet" $ do
    playerWon startingBoard Bottom 0 0 `shouldBe` Nothing
