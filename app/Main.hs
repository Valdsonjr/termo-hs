{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad (forM_, mzero, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.ST.Strict (ST)
import Control.Monad.State
  ( StateT,
    evalStateT,
    gets,
    modify,
    modify',
  )
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Array (Array, (!))
import Data.Array.ST
  ( MArray (newArray),
    STArray,
    readArray,
    runSTArray,
    writeArray,
  )
import Data.Foldable (Foldable (toList), foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Pretty
  ( Color (Blue, Green, White, Yellow),
    Style (Bold),
    bgColor,
    color,
    style,
  )
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (newStdGen, uniformR)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  wordMap <- getWordMap
  gen <- newStdGen
  let ix = fst $ uniformR (0, length wordMap - 1) gen

  when (ix < 0) $ error "Lista de palavras não encontrada"

  T.putStrLn introString

  evalStateT (loop $ runMaybeT game) $
    GS
      { _attemptMap =
          M.fromList $
            map (\letter -> (letter, Untested)) ['A' .. 'Z'],
        _guesses = 1,
        _wordMap = wordMap,
        _answer = fst $ M.elemAt ix wordMap,
        _maxGuesses = 6
      }

data GameState = GS
  { _attemptMap :: !(M.Map Char CharacterStatus),
    _guesses :: !Word,
    _wordMap :: !(M.Map T.Text T.Text),
    _answer :: !T.Text,
    _maxGuesses :: !Word
  }

data CharacterStatus
  = Untested
  | DoesntExist
  | WrongPlace
  | RightPlace
  deriving (Show, Eq, Ord)

getWordMap :: IO (M.Map T.Text T.Text)
getWordMap = do
  allWords <- T.lines <$> T.readFile "palavras.txt"
  pure $ M.fromList $ map (T.map normalizeAccents &&& id) allWords
  where
    normalizeAccents 'Á' = 'A'
    normalizeAccents 'À' = 'A'
    normalizeAccents 'Ã' = 'A'
    normalizeAccents 'Â' = 'A'
    normalizeAccents 'É' = 'E'
    normalizeAccents 'Ê' = 'E'
    normalizeAccents 'Í' = 'I'
    normalizeAccents 'Õ' = 'O'
    normalizeAccents 'Ó' = 'O'
    normalizeAccents 'Ô' = 'O'
    normalizeAccents 'Ú' = 'U'
    normalizeAccents 'Ç' = 'C'
    normalizeAccents cha = cha

introString :: T.Text
introString =
  "Bem vinda(o) ao Termo.hs!\nDigite "
    <> color Green ":?"
    <> " para ajuda, "
    <> color Green ":l"
    <> " para ver as letras adivinhadas, ou "
    <> color Green ":s"
    <> " para sair."

loop :: (Monad m) => m (Maybe a) -> m a
loop action = action >>= maybe (loop action) pure

continue :: Game a
continue = mzero

printLnS :: (MonadIO m) => T.Text -> m ()
printLnS = liftIO . T.putStrLn

type Game a = MaybeT (StateT GameState IO) a

game :: Game ()
game = do
  displayAttemptNumbers

  let drawHelp = printLnS helpString
  let drawAttemptMap = gets _attemptMap >>= printLnS . showAttemptMap

  line <- liftIO T.getLine
  case line of
    ":s" -> pure ()
    ":?" -> drawHelp >> continue
    ":l" -> drawAttemptMap >> continue
    word -> makeAttempt $ T.toUpper word

makeAttempt :: T.Text -> Game ()
makeAttempt word = do
  wordMap <- gets _wordMap

  if M.notMember word wordMap
    then do
      printLnS "Palavra inválida, por favor tente novamente"
      continue
    else do
      answer <- gets _answer
      guesses <- gets _guesses
      let attemptResult = showAttempt word answer
      printLnS $ renderAttempt word attemptResult
      let updm = updateAttemptMap word attemptResult . _attemptMap
      modify' (\s -> s {_attemptMap = updm s})
      maxGuesses <- gets _maxGuesses

      let msg = "A palavra era '" <> wordMap M.! answer <> "'"

      if word == answer
        then do
          printLnS $ "Você ganhou! " <> msg
          pure ()
        else
          if guesses >= maxGuesses
            then do
              printLnS $ "Você perdeu! " <> msg
              pure ()
            else do
              modify (\s -> s {_guesses = _guesses s + 1})
              continue

helpString :: T.Text
helpString =
  style Bold "Regras\n\n"
    <> "Você tem 6 tentativas para adivinhar a palavra. Cada\n"
    <> "tentativa deve ser uma palavra de 5 letras válida.\n\n"
    <> "Após cada tentativa, as cores das letras\n"
    <> "indicarão quão próxima a tentativa está da resposta.\n\n"
    <> "Ignore acentuação e cedilha.\n\n"
    <> style Bold "Exemplos\n\n"
    <> colour RightPlace " M "
    <> " A N G A \nA letra"
    <> color Green " M "
    <> "existe na palavra e está na posição correta.\n\n"
    <> " V "
    <> colour WrongPlace " I "
    <> " O  L  A \nA letra"
    <> color Yellow " I "
    <> "existe na palavra mas em outra posição.\n\n"
    <> " P  L  U "
    <> colour DoesntExist " M "
    <> " A \nA letra M não existe na palavra.\n"

colour :: CharacterStatus -> T.Text -> T.Text
colour Untested = id
colour DoesntExist = style Bold . color Blue . bgColor White
colour WrongPlace = style Bold . color White . bgColor Yellow
colour RightPlace = style Bold . color White . bgColor Green

showAttemptMap :: M.Map Char CharacterStatus -> T.Text
showAttemptMap amap = T.concatMap showColoredChar letters
  where
    letters = "QWERTYUIOP\nASDFGHJKL\n ZXCVBNM"
    showColoredChar c =
      colour
        (M.findWithDefault Untested c amap)
        (showPrettyChar c)

displayAttemptNumbers :: Game ()
displayAttemptNumbers = do
  currentGuess <- gets _guesses
  maxGuesses <- gets _maxGuesses
  liftIO . putStr $
    "Digite sua tentativa ["
      <> show currentGuess
      <> "/"
      <> show maxGuesses
      <> "]: "

showPrettyChar :: Char -> T.Text
showPrettyChar c = T.cons ' ' $ T.cons c " "

showAttempt :: T.Text -> T.Text -> Array Int CharacterStatus
showAttempt attempt answer = runSTArray $ do
  let n = T.length answer - 1
  res <- newArray (0, n) DoesntExist
  amap <- newArray ('A', 'Z') 0 :: ST s (STArray s Char Int)

  forM_ (T.unpack answer) $ \c -> do
    val <- readArray amap c
    writeArray amap c (val + 1)

  forM_ [0 .. n] $ \i -> do
    let answerC = T.index answer i
    let attemptC = T.index attempt i
    when (answerC == attemptC) $ do
      writeArray res i RightPlace
      val <- readArray amap answerC
      writeArray amap answerC (val - 1)

  forM_ [0 .. n] $ \i -> do
    let answerC = T.index answer i
    let attemptC = T.index attempt i
    val <- readArray amap attemptC
    when (answerC /= attemptC && val > 0) $ do
      writeArray amap attemptC (val - 1)
      writeArray res i WrongPlace

  pure res

renderAttempt :: T.Text -> Array Int CharacterStatus -> T.Text
renderAttempt word arr =
  T.concat $
    zipWith
      (\c s -> colour s $ showPrettyChar c)
      (T.unpack word)
      (toList arr)

updateAttemptMap ::
  T.Text ->
  Array Int CharacterStatus ->
  M.Map Char CharacterStatus ->
  M.Map Char CharacterStatus
updateAttemptMap word res amap =
  foldl' (\acc (n, c) -> M.adjust (\cha -> max cha $ res ! n) c acc) amap $
    zip [0 ..] $
      T.unpack word
