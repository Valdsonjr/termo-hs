{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad (forM_, mzero, when)
import Control.Monad.ST.Strict (ST)
import Control.Monad.State
  ( StateT,
    evalStateT,
    gets,
    lift,
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
        _validWords = wordMap,
        _answer = fst $ M.elemAt ix wordMap,
        _maxGuesses = 6,
        _writeStr = T.putStr,
        _readInput = T.getLine
      }

data GameState m = GS
  { _attemptMap :: !(M.Map Char CharacterStatus),
    _guesses :: !Word,
    _validWords :: !(M.Map T.Text T.Text),
    _answer :: !T.Text,
    _maxGuesses :: !Word,
    _writeStr :: T.Text -> m (),
    _readInput :: m T.Text
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

loop :: (Monad m, Monoid b) => m (Maybe b) -> m b
loop action = action >>= maybe (pure mempty) (\x -> mappend x <$> loop action)

exit :: (Monad m) => GameT m ()
exit = mzero

printLnS :: (Monad m) => T.Text -> GameT m ()
printLnS = printS . (<> "\n")

printS :: (Monad m) => T.Text -> GameT m ()
printS txt = do
  writer <- gets _writeStr
  lift $ lift $ writer txt

getLineS :: (Monad m) => GameT m T.Text
getLineS = do
  reader <- gets _readInput
  lift $ lift reader

type GameT m a = MaybeT (StateT (GameState m) m) a

game :: (Monad m) => GameT m ()
game = do
  displayAttemptNumbers

  let drawHelp = printLnS helpString
  let drawAttemptMap = gets _attemptMap >>= printLnS . showAttemptMap

  line <- getLineS
  case line of
    ":s" -> exit
    ":?" -> drawHelp
    ":l" -> drawAttemptMap
    word -> makeAttempt $ T.toUpper word

makeAttempt :: (Monad m) => T.Text -> GameT m ()
makeAttempt word = do
  wordMap <- gets _validWords

  if M.notMember word wordMap
    then printLnS "Palavra inválida, por favor tente novamente"
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
          exit
        else
          if guesses >= maxGuesses
            then do
              printLnS $ "Você perdeu! " <> msg
              exit
            else modify (\s -> s {_guesses = _guesses s + 1})

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

displayAttemptNumbers :: (Monad m) => GameT m ()
displayAttemptNumbers = do
  currentGuess <- gets _guesses
  maxGuesses <- gets _maxGuesses
  printS $
    T.pack $
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
