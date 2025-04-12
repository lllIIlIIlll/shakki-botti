import Test.Hspec
import Data.Vector as V

import Types (GameState(..), Move(..), Color(..))
import Board (fenToGameState, updateGameState, playMove)
import Engine (findBestMove, evaluateBoard)

main :: IO ()
main = hspec $ do
  describe "gamestate operations" $ do
    it "returns empty board gamestate correctly" $ do
      let gameState = fenToGameState "8/8/8/8/8/8/8/8 w KQkq - 0 0"

      let gameStateToCreate = 
        GameState { board        = V.replicate 64 0,
                    turn         = White,
                    wKingCastle  = True,
                    wQueenCastle = True,
                    bKingCastle  = True,
                    bQueenCastle = True,
                    enPassant    = Nothing,
                    halfMove     = 0,
                    fullMove     = 0
                  }
      gameState `shouldBe` V.gameStateToCreate

    it "correctly updates the gamestate after a move" $ do
      let currentGameState = fenToGameState "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"
      let gameStateAfterMove = fenToGameState "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 0"
      let move = Move { from      = (6, 4),
                        dest      = (4, 4),
                        promotion = Nothing,
                        capture   = Nothing,
                        castle    = Nothing }
      updateGameState currentGameState move `shouldBe` gameStateAfterMove
  
  describe "finding checkmates" $ do
    it "finds a mate in one" $ do
      let m1GameState = fenToGameState "8/4Pn2/8/5b2/8/P7/KPpk4/N7 b - - 0 1"
      let moveToPlay = Move { from      = (6, 2),
                              dest      = (7, 2),
                              promotion = Just 'n',
                              capture   = Nothing,
                              castle    = Nothing }
      findBestMove (board m1GameState) (turn m1GameState) `shouldBe` (-100005, Just moveToPlay)

    it "finds a mate in two" $ do
      let m2GameState = fenToGameState "7k/q7/6Q1/6R1/8/7n/6PP/1R5K b - - 0 1"
      let moveToPlay = Move { from      = (1, 0),
                              dest      = (7, 6),
                              promotion = Nothing,
                              capture   = Nothing,
                              castle    = Nothing }
      findBestMove (board m2GameState) (turn m2GameState) `shouldBe` (-100003, Just moveToPlay)

    it "finds a mate in 3" $ do
      let m3GameState = fenToGameState "8/8/8/8/8/2k5/3r4/2K5 b - - 0 1"
      let moveToPlay = Move { from      = (6, 3),
                              dest      = (4, 3),
                              promotion = Nothing,
                              capture   = Nothing,
                              castle    = Nothing }
      findBestMove (board m3GameState) (turn m3GameState) `shouldBe` (-100001, Just moveToPlay)
    
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
