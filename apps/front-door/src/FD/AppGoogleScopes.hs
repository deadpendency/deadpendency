{-# LANGUAGE DataKinds #-}

module FD.AppGoogleScopes (AppGoogleScopes) where

type AppGoogleScopes =
  '[ "https://www.googleapis.com/auth/cloud-platform",
     "https://www.googleapis.com/auth/pubsub",
     "https://www.googleapis.com/auth/logging.admin",
     "https://www.googleapis.com/auth/logging.write",
     "https://www.googleapis.com/auth/trace.append",
     "https://www.googleapis.com/auth/monitoring",
     "https://www.googleapis.com/auth/monitoring.write"
   ]
