{-# LANGUAGE OverloadedStrings #-}

module Game where

import Control.Arrow ((&&&))
import Control.Monad (forM_, mzero, when)
import Control.Monad.RWS.CPS (RWST, asks, evalRWST, gets, modify', runRWST)
import Control.Monad.ST.Strict (ST)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Array.IArray (Array, (!))
import Data.Array.ST (STArray, newArray, readArray, runSTArray, writeArray)
import Data.Foldable (foldl', toList)
import qualified Data.Map.Strict as M
import Data.Text (pack)
import qualified Data.Text as T
import System.Console.Pretty
  ( Color (Blue, Green, White, Yellow),
    Style (Bold),
    bgColor,
    color,
    style,
  )

data GameEnv m = GE
  { _maxGuesses :: !Word,
    _writeStr :: T.Text -> m (),
    _readInput :: m T.Text,
    _validWords :: !(M.Map T.Text T.Text),
    _answer :: !T.Text
  }

data GameState = GS
  { _guesses :: !Word,
    _attemptMap :: !(M.Map Char CharacterStatus)
  }

data CharacterStatus
  = Untested
  | DoesntExist
  | WrongPlace
  | RightPlace
  deriving (Eq, Ord)

mkGameEnv ::
  Word ->
  (T.Text -> m ()) ->
  m T.Text ->
  [T.Text] ->
  T.Text ->
  Either String (GameEnv m)
mkGameEnv maxGuesses writeStr readInput validWords answer =
  GE
    <$> validateMaxGuesses
    <*> pure writeStr
    <*> pure readInput
    <*> pure wordMap
    <*> validateAnswer
  where
    wordMap = M.fromList $ map (normalizeAccents &&& id) validWords
    validateMaxGuesses =
      if maxGuesses > 0
        then Right maxGuesses
        else Left "Número máximo de tentativas deve ser maior que 0"
    validateAnswer =
      let normalizedAnswer = normalizeAccents answer
       in if M.member normalizedAnswer wordMap
            then Right normalizedAnswer
            else Left "Resposta deve ser uma palavra válida"

normalizeAccents :: T.Text -> T.Text
normalizeAccents = T.map normalizeAccent
  where
    normalizeAccent 'Á' = 'A'
    normalizeAccent 'À' = 'A'
    normalizeAccent 'Ã' = 'A'
    normalizeAccent 'Â' = 'A'
    normalizeAccent 'É' = 'E'
    normalizeAccent 'Ê' = 'E'
    normalizeAccent 'Í' = 'I'
    normalizeAccent 'Õ' = 'O'
    normalizeAccent 'Ó' = 'O'
    normalizeAccent 'Ô' = 'O'
    normalizeAccent 'Ú' = 'U'
    normalizeAccent 'Ç' = 'C'
    normalizeAccent cha = cha

evalGame ::
  (Monad m) =>
  GameEnv m ->
  GameState ->
  GameT m a ->
  m (Maybe a, GameState)
evalGame env st g = (\(a, s, _) -> (a, s)) <$> runRWST (runMaybeT g) env st

initialState :: GameState
initialState =
  GS
    { _guesses = 1,
      _attemptMap = M.fromList $ map (\letter -> (letter, Untested)) ['A' .. 'Z']
    }

runGame :: (Monad m) => GameEnv m -> m ()
runGame env = fst <$> evalRWST (loop $ runMaybeT game) env initialState

loop :: (Monad m, Monoid b) => m (Maybe b) -> m b
loop action = action >>= maybe (pure mempty) (\x -> mappend x <$> loop action)

exit :: (Monad m) => GameT m ()
exit = mzero

printLnS :: (Monad m) => T.Text -> GameT m ()
printLnS = printS . (<> "\n")

printS :: (Monad m) => T.Text -> GameT m ()
printS txt = do
  writer <- asks _writeStr
  lift $ lift $ writer txt

getLineS :: (Monad m) => GameT m T.Text
getLineS = asks _readInput >>= lift . lift

type GameT m a = MaybeT (RWST (GameEnv m) () GameState m) a

game :: (Monad m) => GameT m ()
game = do
  displayAttemptNumbers

  let drawAttemptMap = gets _attemptMap >>= printLnS . showAttemptMap

  line <- getLineS
  case line of
    ":s" -> exit
    ":?" -> helpString
    ":l" -> drawAttemptMap
    word -> makeAttempt $ T.toUpper word

makeAttempt :: (Monad m) => T.Text -> GameT m ()
makeAttempt word = do
  wordMap <- asks _validWords

  if not (M.member word wordMap)
    then printLnS "Palavra inválida, por favor tente novamente"
    else do
      answer <- asks _answer
      guesses <- gets _guesses
      let attemptResult = showAttempt word answer
      printLnS $ renderAttempt (wordMap M.! word) attemptResult
      let updm = updateAttemptMap word attemptResult . _attemptMap
      modify' (\s -> s {_attemptMap = updm s})
      maxGuesses <- asks _maxGuesses

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
            else modify' (\s -> s {_guesses = _guesses s + 1})

helpString :: (Monad m) => GameT m ()
helpString = do
  maxGuesses <- asks (pack . show . _maxGuesses)
  printLnS $
    style Bold "Regras\n\n"
      <> "Você tem "
      <> maxGuesses
      <> " tentativas para adivinhar a palavra. Cada\n"
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
  maxGuesses <- asks _maxGuesses
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

introString :: T.Text
introString =
  "Bem vinda(o) ao Termo.hs!\nDigite "
    <> color Green ":?"
    <> " para ajuda, "
    <> color Green ":l"
    <> " para ver as letras adivinhadas, ou "
    <> color Green ":s"
    <> " para sair."
