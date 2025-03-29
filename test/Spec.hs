import Test.Hspec
import Data.Vector as V

import Types (Board, Move(..))
import Board (parseFen, boardToFen, playMove)
import Engine (findBestMove, evaluateBoard)

main :: IO ()
main = hspec $ do
  describe "fen operations" $ do
    it "returns empty board correctly" $ do
      parseFen "8/8/8/8/8/8/8/8" `shouldBe` V.replicate 64 0

    it "updates a move on a board correctly" $ do
      let startPos = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
      let posAfterMove = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR"
      --e2e4
      let move = Move { from = (6, 4),
              dest = (4, 4),
              promotion = Nothing,
              capture = Nothing }

      boardToFen (playMove (parseFen startPos) move) `shouldBe` posAfterMove
  
  describe "finding checkmates" $ do
    it "finds mate in one" $ do
      let m1Pos = "8/4Pn2/8/5b2/8/P7/KPpk4/N7 b - - 0 1"
      findBestMove (parseFen m1Pos) m1Pos `shouldBe` (-100003, "c2c1n")

    it "finds mate on two" $ do
      let m2Pos = "7k/q7/6Q1/6R1/8/7n/6PP/1R5K b - - 0 1"
      findBestMove (parseFen m2Pos) m2Pos `shouldBe` (-100001, "a7g1")
      -- black's only legal answer b1g1
      let posAfter = "7k/8/6Q1/6R1/8/7n/6PP/6RK b - - 0 1"
      findBestMove (parseFen posAfter) posAfter `shouldBe` (-100003, "h3f2")
  
  describe "evaluating board" $ do
    it "evaluates initial board correctly" $ do
      let board = parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
      evaluateBoard board `shouldBe` 0
    
    it "evaluates white's material/position correctly" $ do
      let board = parseFen "8/8/8/8/8/8/PPPPPPPP/RNBQKBNR"
      evaluateBoard board `shouldBe` 3825

    it "evaluates black's material/position correctly" $ do
      let board = parseFen "rnbqkbnr/pppppppp/8/8/8/8/8/8"
      evaluateBoard board `shouldBe` -3825


