module Main (main) where

import Relude.Lifted.Env (lookupEnv)
import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = do
  maybeSmokeTest <- lookupEnv "SMOKE_TEST"
  case maybeSmokeTest of
    Just "true" -> hspecWith defaultConfig Spec.spec
    _ -> putTextLn "\nIgnoring Smoke Test"
