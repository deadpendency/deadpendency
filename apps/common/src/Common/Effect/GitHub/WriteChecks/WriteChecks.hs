{-# LANGUAGE TemplateHaskell #-}

module Common.Effect.GitHub.WriteChecks.WriteChecks
  ( WriteChecks (..),
    createCheckRun,
    updateCheckRun,
  )
where

import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunCreateResult
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateRequest
import Common.Effect.GitHub.WriteChecks.Model.CheckRunUpdateResult
import Control.Effect.TH

data WriteChecks (m :: Type -> Type) k where
  CreateCheckRun :: CheckRunCreateRequest -> WriteChecks m CheckRunCreateResult
  UpdateCheckRun :: CheckRunUpdateRequest -> WriteChecks m CheckRunUpdateResult

makeSmartConstructors ''WriteChecks
