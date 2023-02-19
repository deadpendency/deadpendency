module DD.Effect.DetermineDependencies.Backend.LanguageFiles.Java.BuildGradle
  ( BuildGradleInput (..),
  )
where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Dependency.File.DependenciesFileType qualified as DFT
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Parsing.Megaparsec
import Common.Parsing.NameParsing
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import DD.Effect.DetermineDependencies.Model.DetermineDependenciesError
import Data.Vector qualified as V
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as ML

newtype BuildGradle = BuildGradle
  { _pjBasicDependencies :: V.Vector BasicDependency
  }
  deriving stock (Eq, Show, Generic)

newtype BuildGradleInput = BuildGradleInput
  { _pjText :: Text
  }
  deriving stock (Eq, Show, Generic)

-- Full list of keys https://docs.gradle.org/current/userguide/java_library_plugin.html#sec:java_library_configurations_graph
{-
buildscript {
    dependencies {
        classpath group: 'commons-codec', name: 'commons-codec', version: '1.2'
        classpath("org.springframework.boot:spring-boot-gradle-plugin:$springBootVersion")
        classpath "com.avast.gradle:gradle-docker-compose-plugin:0.9.2"
    }
}

dependencies {
    compile "io.eventuate.client.java:eventuate-client-java-spring:$eventuateClientVersion"

    compile 'mysql:mysql-connector-java:5.1.36'
    compile('org.postgresql:postgresql:9.4-1200-jdbc41') {
        exclude group: 'org.slf4j', module: 'slf4j-simple'
    }

    compile "org.springframework.boot:spring-boot-starter-data-jpa:$springBootVersion"

    implementation 'org.hibernate:hibernate-core:3.6.7.Final'
    api 'com.google.guava:guava:23.0'
    testImplementation 'junit:junit:4.+'

    testCompile "junit:junit:4.12"
    testCompile "io.eventuate.client.java:eventuate-client-java-spring-jdbc:$eventuateClientVersion"
}
-}

parserBuildGradle :: MParser BuildGradle
parserBuildGradle =
  parserDependencies <&> BuildGradle

parserDependencies :: MParser (V.Vector BasicDependency)
parserDependencies = do
  parserUntilDepOrEnd
  deps <- M.many $ ML.lexeme parserUntilDepOrEnd parserSingleDependencyList
  pure $ V.concat deps

parserUntilDepOrEnd :: MParser ()
parserUntilDepOrEnd = do
  M.skipManyTill
    M.anySingle
    (parserEOLWithDepNext <|> M.eof)

parserEOLWithDepNext :: MParser ()
parserEOLWithDepNext = void (M.try $ M.eol *> M.lookAhead depBeginning)

depBeginning :: MParser Text
depBeginning =
  M.hspace *> depKeys

depKeys :: MParser Text
depKeys =
  M.string "api"
    <|> M.string "implementation"
    <|> M.string "classpath"
    <|> M.try (M.string "runtime")
    <|> M.string "runtimeOnly"
    <|> M.try (M.string "compile")
    <|> M.try (M.string "compileOnly")
    <|> M.string "compileOnlyApi"
    <|> M.try (M.string "testCompile")
    <|> M.try (M.string "testCompileOnly")
    <|> M.try (M.string "testImplementation")
    <|> M.try (M.string "testRuntime")
    <|> M.string "testRuntimeOnly"

-- runtimeOnly 'org.springframework:spring-core:2.5',
--         'org.springframework:spring-aop:2.5'
-- runtimeOnly(
--     [group: 'org.springframework', name: 'spring-core', version: '2.5'],
--     [group: 'org.springframework', name: 'spring-aop', version: '2.5']
-- )
parserSingleDependencyList :: MParser (V.Vector BasicDependency)
parserSingleDependencyList = do
  M.hspace
  depKeys
  M.hspace
  M.optional (M.char '(')
  deps <- M.many $ M.try parserOldStyleDep <|> M.try parserNewStyleDep
  pure $ V.fromList deps

-- compile('org.postgresql:postgresql:9.4-1200-jdbc41')
-- testImplementation "junit:junit:4.+"
parserOldStyleDep :: MParser BasicDependency
parserOldStyleDep = do
  namespace <- pack <<$>> M.skipManyTill notNewline $ M.between quote (M.char ':') (M.some depNameExpandedChar)
  name <- pack <$> M.someTill depNameExpandedChar (M.char ':' <|> quote)
  M.skipManyTill M.anySingle endOfLineOrFile
  pure $
    nameToDependency namespace name

-- this won't work with windows newlines :/
notNewline :: MParser Char
notNewline = M.anySingleBut '\n'

-- classpath group: 'commons-codec', name: 'commons-codec', version: '1.2'
parserNewStyleDep :: MParser BasicDependency
parserNewStyleDep = do
  namespace <- pack <<$>> M.lookAhead $ M.skipManyTill M.anySingle $ groupPrefix *> M.hspace *> M.between quote quote (M.some depNameExpandedChar)
  name <- pack <<$>> M.skipManyTill M.anySingle $ namePrefix *> M.hspace *> M.between quote quote (M.some depNameExpandedChar)
  M.skipManyTill M.anySingle endOfLineOrFile
  pure $
    nameToDependency namespace name

groupPrefix :: MParser ()
groupPrefix = void $ M.string "group" *> M.hspace *> (M.char ':' <|> M.char '=')

namePrefix :: MParser ()
namePrefix = void $ M.string "name" *> M.hspace *> (M.char ':' <|> M.char '=')

endOfLineOrFile :: MParser ()
endOfLineOrFile = void M.eol <|> M.eof

quote :: MParser Char
quote = M.char '\'' <|> M.char '\"'

nameToDependency :: Text -> Text -> BasicDependency
nameToDependency namespace name =
  BasicDependency
    Java
    (DependencyIdentifierNamed (DependencyName $ namespace <> "/" <> name))
    Nothing

instance CanDetermineDependencies BuildGradleInput where
  determineDependencies gitPath =
    bimap
      (UnableToParseDependencyFile DFT.BuildGradle gitPath . pack . M.errorBundlePretty)
      _pjBasicDependencies
      . M.parse parserBuildGradle "BuildGradle"
      . _pjText
