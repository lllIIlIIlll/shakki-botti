module Main (main) where

import Engine (findBestMove)
import LegalMoves (findLegalMoves)
import Board (fenToGameState, updateGameState)
import Utils (coordToSquare, squareToCoord)
import Types (GameState(..), Move(..))

import System.IO (hFlush, stdout)
import Data.List.Split(splitOn)
import Data.List()
import Data.Char
import Data.IORef


setGameState :: IORef GameState -> GameState -> IO ()
setGameState gameState newGameState = do
  writeIORef gameState newGameState

mainLoop :: IORef GameState -> IO ()
mainLoop gameState = do
    input <- getLine

    let tagAndData = splitOn ":" input
    let tag = head tagAndData
    let resData = last tagAndData

    case tag of
      "BOARD" -> do
        let newGameState = fenToGameState resData
        putStrLn resData
        hFlush stdout

        setGameState gameState newGameState
        mainLoop gameState

      "RESET" -> do
        let newGameState = fenToGameState "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        putStrLn "reset"
        hFlush stdout

        setGameState gameState newGameState
        mainLoop gameState

      "PLAY" -> do
        currentGameState <- readIORef gameState
        
        -- for debugging
        -- let legalMoves = findLegalMoves (board currentGameState) (turn currentGameState)
        -- let ordLegalMoves = sortBy (comparing (Down . capture)) legalMoves
        -- putStrLn ("These are black's legal moves: " ++ show ordLegalMoves)

        let (eval, move) = findBestMove (board currentGameState) (turn currentGameState)
        case move of
          Just move' -> do
            let moveStr = coordToSquare (from move') ++ coordToSquare (dest move')  
            let fullMoveStr = case promotion move' of
                                Just p  -> moveStr ++ [p]
                                Nothing -> moveStr

            putStrLn ("Eval: " ++ show eval)
            putStrLn ("Black chose the move: " ++ fullMoveStr)
            hFlush stdout

            putStrLn ("MOVE:" ++ fullMoveStr)
            hFlush stdout

            let newGameState = updateGameState currentGameState move'
            setGameState gameState newGameState
            mainLoop gameState

          Nothing -> do
            putStrLn "GAME OVER"

      "MOVE" -> do
        currentGameState <- readIORef gameState

        -- promotion move's last char is always lowercase
        let correctedMove = if length resData == 5
                            then Move { from      = squareToCoord (take 2 resData),
                                        dest      = squareToCoord (take 2 (drop 2 resData)),
                                        promotion = Just (toUpper (resData !! 4)),
                                        capture   = Nothing,
                                        castle    = Nothing }
                            else Move { from      = squareToCoord (take 2 resData),
                                        dest      = squareToCoord (take 2 (drop 2 resData)),
                                        promotion = Nothing,
                                        capture   = Nothing,
                                        castle    = Nothing }

        putStrLn ("White chose the move: " ++ resData)
        hFlush stdout

        let newGameState = updateGameState currentGameState correctedMove
        setGameState gameState newGameState
        mainLoop gameState

      _ -> do
        putStrLn "unknown tag"
        return ()

main :: IO ()
main = do
  putStrLn "lets play chess"
  hFlush stdout
  gameState <- newIORef (fenToGameState "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
  mainLoop gameState
