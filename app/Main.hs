module Main (main) where

import Engine (findLegalMoves)
import Board (parseFen, updateFen)

import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import Data.IORef

-- testing purposes
import System.Random (randomRIO)

getRandomMove :: [a] -> IO a
getRandomMove xs = do
  index <- randomRIO (0, length xs - 1)
  return (xs !! index)
-- testing purposes

setPosition :: IORef String -> String -> IO ()
setPosition boardRef position = do
  writeIORef boardRef position
  putStrLn ("set board to: " ++ position)
  hFlush stdout

mainLoop :: IORef String -> IO ()
mainLoop boardRef = do
    input <- getLine

    let tagAndData = splitOn ":" input
    let tag = head tagAndData
    let resData = last tagAndData

    case tag of
      "BOARD" -> do
        let newFen = resData
        setPosition boardRef newFen
        mainLoop boardRef

      "RESET" -> do
        let startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        putStrLn ("board reset: " ++ startFen)
        hFlush stdout
        setPosition boardRef startFen
        mainLoop boardRef

      "PLAY" -> do
        currentFen <- readIORef boardRef
        let legalMoves = findLegalMoves (parseFen currentFen) currentFen
        putStrLn ("These are black's legal moves: " ++ show legalMoves)
        move <- getRandomMove legalMoves -- random for testing purposes
        putStrLn ("Black chose the move: " ++ move)
        hFlush stdout

        putStrLn ("MOVE:" ++ move)
        hFlush stdout

        let newBoardRef = updateFen currentFen move
        setPosition boardRef newBoardRef
        mainLoop boardRef

      "MOVE" -> do
        currentFen <- readIORef boardRef
        let legalMoves = findLegalMoves (parseFen currentFen) currentFen
        putStrLn ("These are white's legal moves: " ++ show legalMoves)
        hFlush stdout

        putStrLn ("White chose the move: " ++ resData)
        hFlush stdout

        let newBoardRef = updateFen currentFen resData
        setPosition boardRef newBoardRef
        mainLoop boardRef

      _ -> do
        putStrLn "unknown tag"
        return ()

main :: IO ()
main = do
  putStrLn "lets play chess"
  hFlush stdout
  boardRef <- newIORef "startpos"
  mainLoop boardRef
