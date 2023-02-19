module Effect.DetermineDependencies.Backend.LanguageFiles.Java.BuildGradleSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Java.BuildGradle
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a BuildGradle file" $ do
      it "determines happy day dependencies" $ do
        let inputRequirements =
              [r|
dependencies {
    implementation 'org.hibernate:hibernate-core:3.6.7.Final'
    api 'com.google.guava:guava:23.0'
    testImplementation 'junit:junit:4.+'
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate-core") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "com.google.guava/guava") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "junit/junit") Nothing
                  ]

        result `shouldBe` expected

      it "works with new style deps" $ do
        let inputRequirements =
              [r|
dependencies {
    runtimeOnly group: 'org.hibernate', name: 'hibernate-core', version: '3.0.5', transitive: true
    runtimeOnly(group: 'org.hibernate', name: 'hibernate', version: '3.0.5') {
      	transitive = true
    }
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate-core") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate") Nothing
                  ]

        result `shouldBe` expected

      it "handles multiple deps per key in old style" $ do
        let inputRequirements =
              [r|
dependencies {
  	runtimeOnly 'org.springframework:spring-core:2.5',
          	'org.springframework:spring-aop:2.5'
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.springframework/spring-core") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.springframework/spring-aop") Nothing
                  ]

        result `shouldBe` expected

      it "handles multiple deps per key in new style" $ do
        let inputRequirements =
              [r|
dependencies {
  	runtimeOnly(
      	[group: 'org.springframework', name: 'spring-core', version: '2.5'],
        [group: 'org.springframework', name: 'spring-aop', version: '2.5']
    )
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.springframework/spring-core") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.springframework/spring-aop") Nothing
                  ]

        result `shouldBe` expected

      it "gracefully handles key matches without valid deps" $ do
        let inputRequirements =
              [r|
dependencies {
    runtimeOnly group: 'org.hibernate', name: 'hibernate-core', version: '3.0.5', transitive: true
    runtimeOnly not valid for some reason should gracefully ignore
    runtimeOnly(group: 'org.hibernate', name: 'hibernate', version: '3.0.5') {
        transitive = true
    }
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate-core") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate") Nothing
                  ]

        result `shouldBe` expected

      it "handles kotlin style gradle deps" $ do
        let inputRequirements =
              [r|
dependencies {
    runtimeOnly(group = "org.springframework", name = "spring-core", version = "2.5")
    runtimeOnly("org.springframework:spring-aop:2.5")
  	runtimeOnly("org.hibernate:hibernate:3.0.5") {
      	isTransitive = true
    }
    runtimeOnly(group = "org.hibernate", name = "hibernate-core", version = "3.0.5") {
        isTransitive = true
    }
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.springframework/spring-core") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.springframework/spring-aop") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.hibernate/hibernate-core") Nothing
                  ]

        result `shouldBe` expected

      it "matches on buildscript deps" $ do
        let inputRequirements =
              [r|
buildscript {
    repositories {
      	mavenCentral()
    }
    dependencies {
      	classpath group: 'commons-codec', name: 'commons-codec', version: '1.2'
    }
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "commons-codec/commons-codec") Nothing
                  ]

        result `shouldBe` expected

      it "confirm random : or quote won't break things" $ do
        let inputRequirements =
              [r|
buildscript {
  dependencies {
    classpath "org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin_version"
  }
}

apply plugin: 'kotlin'

dependencies {
  compile "org.jetbrains.kotlin:kotlin-stdlib:$kotlin_version"
  testCompile 'junit:junit:4.11'
  testCompile "org.jetbrains.kotlin:kotlin-test-junit:$kotlin_version"
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.jetbrains.kotlin/kotlin-gradle-plugin") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.jetbrains.kotlin/kotlin-stdlib") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "junit/junit") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.jetbrains.kotlin/kotlin-test-junit") Nothing
                  ]

        result `shouldBe` expected

      it "windows newlines should work" $ do
        let inputRequirements =
              "\r\nbuildscript {\r\n  dependencies {\r\n    classpath \"org.jetbrains.kotlin:kotlin-gradle-plugin:$kotlin_version\"\r\n  }  \r\n}\r\n\r\ndependencies {\r\n  compile \"org.jetbrains.kotlin:kotlin-stdlib:$kotlin_version\"\r\n  }\r\n"
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.jetbrains.kotlin/kotlin-gradle-plugin") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.jetbrains.kotlin/kotlin-stdlib") Nothing
                  ]

        result `shouldBe` expected

      it "random comments with keywords don't through it off" $ do
        let inputRequirements =
              [r|

dependencies {
  // compile ? shound't this be testCompile?
  compile "org.jetbrains.kotlin:kotlin-stdlib:$kotlin_version"
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.jetbrains.kotlin/kotlin-stdlib") Nothing
                  ]

        result `shouldBe` expected

      it "works with no dependencies" $ do
        let inputRequirements =
              [r|
no deps anywhere here
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "tabs work" $ do
        let inputRequirements =
              [r|

dependencies {
			compile "org.jetbrains.kotlin:kotlin-stdlib:$kotlin_version"
}
|]
            input = BuildGradleInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.jetbrains.kotlin/kotlin-stdlib") Nothing
                  ]

        result `shouldBe` expected
