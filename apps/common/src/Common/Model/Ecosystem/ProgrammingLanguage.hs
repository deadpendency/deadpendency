{-# LANGUAGE DeriveAnyClass #-}

module Common.Model.Ecosystem.ProgrammingLanguage
  ( ProgrammingLanguage (..),
    plToPretty,
    plFromPretty,
    plToRegistry,
  )
where

import Common.Aeson.Aeson
import Common.Model.Ecosystem.Registry
import Data.Aeson

data ProgrammingLanguage
  = JavaScript
  | TypeScript
  | Python
  | Php
  | Ruby
  | Haskell
  | Rust
  | CSharpNet
  | VisualBasicNet
  | Java
  | Kotlin
  | Scala
  | Golang
  | UnsupportedLanguage Text
  deriving stock (Eq, Show, Generic, Ord)
  deriving anyclass (NFData)

instance ToJSON ProgrammingLanguage where
  toJSON = genericToJSON cleanJSONOptions
  toEncoding = genericToEncoding cleanJSONOptions

instance FromJSON ProgrammingLanguage where
  parseJSON = genericParseJSON cleanJSONOptions

plToPretty :: ProgrammingLanguage -> Text
plToPretty =
  \case
    JavaScript -> "JavaScript"
    TypeScript -> "TypeScript"
    Python -> "Python"
    Php -> "PHP"
    Ruby -> "Ruby"
    Haskell -> "Haskell"
    Rust -> "Rust"
    CSharpNet -> "C# .NET"
    VisualBasicNet -> "Visual Basic .NET"
    Java -> "Java"
    Kotlin -> "Kotlin"
    Scala -> "Scala"
    Golang -> "Golang"
    UnsupportedLanguage other -> other

plFromPretty :: Text -> ProgrammingLanguage
plFromPretty =
  \case
    "JavaScript" -> JavaScript
    "TypeScript" -> TypeScript
    "Python" -> Python
    "PHP" -> Php
    "Ruby" -> Ruby
    "Haskell" -> Haskell
    "Rust" -> Rust
    "C# .NET" -> CSharpNet
    "Visual Basic .NET" -> VisualBasicNet
    "Java" -> Java
    "Kotlin" -> Kotlin
    "Scala" -> Scala
    "Golang" -> Golang
    other -> UnsupportedLanguage other

plToRegistry :: ProgrammingLanguage -> Maybe Registry
plToRegistry =
  \case
    JavaScript -> Just Npm
    TypeScript -> Just Npm
    Python -> Just Pypi
    Php -> Just Packagist
    Ruby -> Just RubyGems
    Haskell -> Just Hackage
    Rust -> Just Crates
    CSharpNet -> Just NuGet
    VisualBasicNet -> Just NuGet
    Java -> Just Maven
    Kotlin -> Just Maven
    Scala -> Just Maven
    Golang -> Just PkgGoDev
    UnsupportedLanguage _ -> Nothing
