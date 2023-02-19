{-# LANGUAGE BangPatterns #-}

module Main
  ( main,
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import DF.Effect.FetchDependencies.Model.FetchDependenciesError
import DF.Effect.FetchRegistryRepoInfo.Backend.LanguageRegistryFiles.Npm.Npm
import DF.Effect.FetchRegistryRepoInfo.Backend.RegistryFetchOrchestrateBackend
import DF.Effect.FetchRegistryRepoInfo.Model.FetchRegistryRepoInfoResult
import Data.Vector.NonEmpty qualified as NV
import Streamly
import Streamly.Prelude qualified as S
import Weigh

main :: IO ()
main =
  void $ getAllRegistryInfo dependencies

getAllRegistryInfo ::
  NV.NonEmptyVector BasicDependency ->
  IO (Either FetchDependenciesError (NV.NonEmptyVector FetchRegistryRepoInfoResult))
getAllRegistryInfo input = do
  result <- fmap sequenceA $ S.foldl' (\acc !ri -> ri : acc) [] $ maxThreads 3 $ aheadly $ S.mapM getRegistryInfo $ S.fromFoldable input
  pure $
    result
      >>= \deps ->
        case NV.fromList deps of
          Just nevDeps -> Right nevDeps
          _ -> Left UnexpectedEmptyDependenciesInStream

dependencies :: NV.NonEmptyVector BasicDependency
dependencies =
  NV.unsafeFromList
    [ toBD "react-native-document-picker",
      toBD "react-hook-form",
      toBD "react-native-swipe-list-view",
      toBD "@react-native-async-storage/async-storage",
      toBD "redux-saga",
      toBD "react-redux",
      toBD "moment-range",
      toBD "react-native-fast-image",
      toBD "react-native-uuid",
      toBD "color",
      toBD "@react-navigation/native",
      toBD "react-native-floating-action",
      toBD "rn-fetch-blob",
      toBD "react-native-modal",
      toBD "react-i18next",
      toBD "react-native-device-info",
      toBD "@react-native-community/datetimepicker",
      toBD "redux",
      toBD "react-native-calendars",
      toBD "react-content-loader",
      toBD "react-native-ios-context-menu",
      toBD "react-native-sectioned-multi-select",
      toBD "react-native-animatable",
      toBD "react-native-image-picker",
      toBD "redux-toolkit-saga",
      toBD "axios",
      toBD "@react-native-community/hooks",
      toBD "@react-navigation/stack",
      toBD "react-native-screens",
      toBD "@react-native-picker/picker",
      toBD "i18next",
      toBD "react-native-pdf",
      toBD "query-string",
      toBD "fuzzy",
      toBD "react-native-swiper",
      toBD "@react-native-community/blur",
      toBD "react-native",
      toBD "lodash",
      toBD "@react-native-community/netinfo",
      toBD "react-native-reanimated",
      toBD "react-native-keyboard-aware-scroll-view",
      toBD "buffer",
      toBD "react-native-permissions",
      toBD "@react-native-firebase/app",
      toBD "react-native-contacts",
      toBD "react-native-paper",
      toBD "typed-redux-saga",
      toBD "react-native-code-push",
      toBD "@rnhooks/keyboard",
      toBD "@react-native-community/masked-view",
      toBD "@react-navigation/material-bottom-tabs",
      toBD "react-native-safe-area-context",
      toBD "react-native-loading-spinner-overlay",
      toBD "@react-native-firebase/analytics",
      toBD "react-native-popup-menu",
      toBD "react-native-vector-icons",
      toBD "react-native-orientation-locker",
      toBD "pretty-bytes",
      toBD "react-native-share",
      toBD "@sentry/react-native",
      toBD "jwt-decode",
      toBD "react-native-highlight-words",
      toBD "react-native-auth0",
      toBD "reanimated-bottom-sheet",
      toBD "moment",
      toBD "react-native-bootsplash",
      toBD "react-native-toast-message",
      toBD "@sentry/cli",
      toBD "@reduxjs/toolkit",
      toBD "symbol-observable",
      toBD "react",
      toBD "react-native-offline",
      toBD "@react-native-firebase/messaging",
      toBD "react-native-autogrow-textinput",
      toBD "react-native-gesture-handler",
      toBD "react-native-svg",
      toBD "react-native-image-zoom-viewer",
      toBD "react-native-fs",
      toBD "react-native-draggable-flatlist",
      toBD "react-native-dropdown-picker",
      toBD "@notifee/react-native",
      toBD "react-native-appstate-hook",
      toBD "@gorhom/portal",
      toBD "@ptomasroos/react-native-multi-slider",
      toBD "redux-devtools-extension",
      toBD "@react-native-community/progress-bar-android",
      toBD "react-native-action-view",
      toBD "@react-navigation/drawer",
      toBD "react-native-context-menu-view",
      toBD "redux-persist",
      toBD "native-base",
      toBD "@react-native-community/progress-view",
      toBD "react-native-swipe-gestures",
      toBD "react-native-camera",
      toBD "react-native-calendar-picker",
      toBD "react-native-snap-carousel",
      toBD "@types/react-native-auth0",
      toBD "eslint-plugin-jsx-a11y",
      toBD "@types/react-native-calendars",
      toBD "eslint-plugin-import",
      toBD "jest",
      toBD "@typescript-eslint/eslint-plugin",
      toBD "@types/react-native-uuid",
      toBD "typescript",
      toBD "detox",
      toBD "eslint-config-airbnb-typescript",
      toBD "@types/react-native-calendar-picker",
      toBD "@types/react-native-loading-spinner-overlay",
      toBD "@babel/runtime",
      toBD "@types/lodash",
      toBD "eslint-plugin-react",
      toBD "@types/react-native",
      toBD "metro-react-native-babel-preset",
      toBD "@types/react-native-highlight-words",
      toBD "@types/jest",
      toBD "react-test-renderer",
      toBD "@react-native-community/eslint-config",
      toBD "eslint-config-prettier",
      toBD "eslint-plugin-react-native",
      toBD "@types/react-native-vector-icons",
      toBD "eslint",
      toBD "@types/react-native-share",
      toBD "react-native-svg-transformer",
      toBD "@babel/core",
      toBD "styled-components",
      toBD "react-native-clean-project",
      toBD "babel-jest",
      toBD "postinstall-postinstall",
      toBD "@types/react-native-snap-carousel",
      toBD "@typescript-eslint/parser",
      toBD "styled-system",
      toBD "@types/react-test-renderer",
      toBD "@types/react-redux",
      toBD "prettier",
      toBD "patch-package"
    ]

toBD :: Text -> BasicDependency
toBD depName =
  BasicDependency
    JavaScript
    (DependencyIdentifierNamed $ DependencyName depName)
    Nothing
