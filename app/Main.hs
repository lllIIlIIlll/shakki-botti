module Main (main) where

import Engine (findBestMove)
import LegalMoves (findLegalMoves)
import Board (parseFen, updateFen)
import Types (Color(..))

import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
import Data.Char
import Data.IORef

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
        let bestMove = findBestMove (parseFen currentFen) currentFen
        let legalMoves = findLegalMoves (parseFen currentFen) Black
        putStrLn ("These are black's legal moves: " ++ show legalMoves)
        putStrLn ("Eval: " ++ show (fst bestMove))
        putStrLn ("Black chose the move: " ++ snd bestMove)
        hFlush stdout

        putStrLn ("MOVE:" ++ snd bestMove)
        hFlush stdout

        let newBoardRef = updateFen currentFen (snd bestMove)
        setPosition boardRef newBoardRef
        mainLoop boardRef

      "MOVE" -> do
        currentFen <- readIORef boardRef
        -- promotion move's last char is always lowercase
        let correctedMove = if length resData == 5
                             then take 4 resData ++ [toUpper (resData !! 4)]
                             else resData
        
        putStrLn ("White chose the move: " ++ correctedMove)
        hFlush stdout

        let newBoardRef = updateFen currentFen correctedMove
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
