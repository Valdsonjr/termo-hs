module Main (main) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Game (introString, mkGameEnv, runGame)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (newStdGen, uniformR)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  allWords <- T.lines <$> T.readFile "palavras.txt"
  gen <- newStdGen
  let ix = fst $ uniformR (0, length allWords - 1) gen

  when (ix < 0) $ error "Lista de palavras não encontrada"

  T.putStrLn introString

  let answer = allWords !! ix
  case mkGameEnv 6 T.putStr T.getLine allWords answer of
    Left err -> error err
    Right env -> runGame env
