module Main (main) where

import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text, pack)
import Game
  ( GameEnv,
    GameState (_guesses),
    evalGame,
    initialState,
    makeAttempt,
    mkGameEnv,
  )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

main :: IO ()
main =
  defaultMain $
    testGroup
      "termo-hs"
      [ testCase "lastTryAttemptSucceeded" lastTryAttemptSucceeded,
        testCase "lastTryAttemptFailed" lastTryAttemptFailed,
        testCase "maxNumberOfGuesses" maxNumberOfGuesses
      ]

defTestGame ::
  (Applicative m) =>
  Word ->
  Text ->
  [Text] ->
  Text ->
  Either String (GameEnv m)
defTestGame n attempt = mkGameEnv n (\_ -> pure ()) (pure attempt)

lastTryAttemptSucceeded :: IO ()
lastTryAttemptSucceeded = do
  let answer = pack "PUDIM"
  let attempt = pack "pudim"
  let mGameEnv = defTestGame 1 attempt [answer] answer
  let game = makeAttempt attempt

  case mGameEnv of
    Left err -> assertFailure err
    Right gameEnv -> do
      let (res, _) = runIdentity $ evalGame gameEnv initialState game
      assertBool "" (isJust res)

maxNumberOfGuesses :: IO ()
maxNumberOfGuesses = do
  let answer = pack "PUDIM"
  let attempt = pack "podio"
  let mGameEnv = defTestGame 6 attempt [answer, attempt] answer
  let game = makeAttempt attempt

  case mGameEnv of
    Left err -> assertFailure err
    Right gameEnv -> do
      let (res0, st0) = runIdentity $ evalGame gameEnv initialState game
      assertBool "" (isJust res0)
      let (res1, st1) = runIdentity $ evalGame gameEnv st0 game
      assertBool "" (isJust res1)
      let (res2, st2) = runIdentity $ evalGame gameEnv st1 game
      assertBool "" (isJust res2)
      let (res3, st3) = runIdentity $ evalGame gameEnv st2 game
      assertBool "" (isJust res3)
      let (res4, st4) = runIdentity $ evalGame gameEnv st3 game
      assertBool "" (isJust res4)
      let (res5, _) = runIdentity $ evalGame gameEnv st4 game
      assertBool "" (isNothing res5)

lastTryAttemptFailed :: IO ()
lastTryAttemptFailed = do
  let answer = pack "PUDIM"
  let attempt = pack "podio"
  let mGameEnv = defTestGame 1 attempt [answer, attempt] answer
  let game = makeAttempt attempt

  case mGameEnv of
    Left err -> assertFailure err
    Right gameEnv -> do
      let (res, st) = runIdentity $ evalGame gameEnv initialState game
      assertBool (show $ _guesses st) (isNothing res)
