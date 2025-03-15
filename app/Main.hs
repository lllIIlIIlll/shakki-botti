module Main (main) where

import System.IO (hFlush, stdout)
import Data.List.Split (splitOn)
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
        putStrLn "MOVE:e7e5"
        hFlush stdout
        mainLoop boardRef
      "MOVE" -> do
        putStrLn ("received move: " ++ resData)
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
