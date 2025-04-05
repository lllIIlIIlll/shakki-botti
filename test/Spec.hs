import Test.Hspec
import Data.Vector as V

import Types (GameState(..), Move(..))
import Board (fenToGameState, updateGameState, playMove)
import Engine (findBestMove, evaluateBoard)

main :: IO ()
main = hspec $ do
  describe "gamestate operations" $ do
    it "returns empty board correctly" $ do
      let emptyBoard = board (fenToGameState "8/8/8/8/8/8/8/8 w KQkq - 0 0")
      emptyBoard `shouldBe` V.replicate 64 0

    it "updates a move on a board correctly" $ do
      let currentGameState = fenToGameState "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
      let gameStateAfterMove = fenToGameState "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 0"
      let move = Move { from      = (6, 4),
                        dest      = (4, 4),
                        promotion = Nothing,
                        capture   = Nothing,
                        castle    = Nothing }
      updateGameState currentGameState move `shouldBe` gameStateAfterMove
  
  describe "finding checkmates" $ do
    it "finds mate in one" $ do
      let m1GameState = fenToGameState "8/4Pn2/8/5b2/8/P7/KPpk4/N7 b - - 0 1"
      let moveToPlay = Move { from      = (6, 2),
                              dest      = (7, 2),
                              promotion = Just 'n',
                              capture   = Nothing,
                              castle    = Nothing }
      findBestMove (board m1GameState) (turn m1GameState) `shouldBe` (-100003, Just moveToPlay)

    it "finds mate on two" $ do
      let m2GameState = fenToGameState "7k/q7/6Q1/6R1/8/7n/6PP/1R5K b - - 0 1"
      let moveToPlay = Move { from      = (1, 0),
                              dest      = (7, 6),
                              promotion = Nothing,
                              capture   = Nothing,
                              castle    = Nothing }
      findBestMove (board m2GameState) (turn m2GameState) `shouldBe` (-100001, Just moveToPlay)
      -- black's only legal answer b1g1
      let gameStateAfter = fenToGameState "7k/8/6Q1/6R1/8/7n/6PP/6RK b - - 0 1"
          moveToPlay = Move { from      = (5, 7),
                              dest      = (6, 5),
                              promotion = Nothing,
                              capture   = Nothing,
                              castle    = Nothing }
      findBestMove (board gameStateAfter) (turn gameStateAfter) `shouldBe` (-100003, Just moveToPlay)
  
  describe "evaluating board" $ do
    it "evaluates initial board correctly" $ do
      let boardToEval = board (fenToGameState "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
      evaluateBoard boardToEval `shouldBe` 0
    
    it "evaluates white's material/position correctly" $ do
      let boardToEval = board (fenToGameState "8/8/8/8/8/8/PPPPPPPP/RNBQKBNR")
      evaluateBoard boardToEval `shouldBe` 3825

    it "evaluates black's material/position correctly" $ do
      let boardToEval = board (fenToGameState "rnbqkbnr/pppppppp/8/8/8/8/8/8")
      evaluateBoard boardToEval `shouldBe` -3825
