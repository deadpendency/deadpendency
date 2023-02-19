{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.CountPrivateInstalls.CountPrivateInstalls
  ( CountPrivateInstalls (..),
    privateInstallsCount,
  )
where

import Common.Effect.GitHub.CountPrivateInstalls.Model.CountPrivateInstallsResult
import Control.Effect.TH

data CountPrivateInstalls (m :: Type -> Type) k where
  PrivateInstallsCount :: CountPrivateInstalls m CountPrivateInstallsResult

makeSmartConstructors ''CountPrivateInstalls
