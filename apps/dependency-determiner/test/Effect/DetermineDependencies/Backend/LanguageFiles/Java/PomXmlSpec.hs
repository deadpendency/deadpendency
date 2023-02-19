module Effect.DetermineDependencies.Backend.LanguageFiles.Java.PomXmlSpec (spec) where

import Common.Model.Dependency.Basic.BasicDependency
import Common.Model.Dependency.DependencyIdentifier
import Common.Model.Dependency.DependencyName
import Common.Model.Ecosystem.ProgrammingLanguage
import Common.Model.Git.GitPath
import DD.Effect.DetermineDependencies.Backend.LanguageFiles.Java.PomXml
import DD.Effect.DetermineDependencies.Backend.Model.CanDetermineDependencies
import Data.Vector qualified as V
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = parallel $
  context "determine deps" $
    describe "of a PomXml file" $ do
      it "determines happy day dependencies" $ do
        let inputRequirements =
              [r|
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <dependencies>
    <!-- AppEngine runtime -->
    <dependency>
      <groupId>com.google.appengine</groupId>
      <artifactId>appengine-api-1.0-sdk</artifactId>
    </dependency>
    <dependency>
      <groupId>org.webjars</groupId>
      <artifactId>bootstrap</artifactId>
    </dependency>
  </dependencies>
</project>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "com.google.appengine/appengine-api-1.0-sdk") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.webjars/bootstrap") Nothing
                  ]

        result `shouldBe` expected

      it "parses all deps from multiple dependencies" $ do
        let inputRequirements =
              [r|
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <dependencies>
    <!-- AppEngine runtime -->
    <dependency>
      <groupId>com.google.appengine</groupId>
      <artifactId>appengine-api-1.0-sdk</artifactId>
    </dependency>
    <dependency>
      <groupId>org.webjars</groupId>
      <artifactId>bootstrap</artifactId>
    </dependency>
  </dependencies>
  <dependencies>
    <dependency>
        <groupId>jstl</groupId>
        <artifactId>jstl</artifactId>
    </dependency>
  </dependencies>
</project>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "com.google.appengine/appengine-api-1.0-sdk") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.webjars/bootstrap") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "jstl/jstl") Nothing
                  ]

        result `shouldBe` expected

      it "loads build plugin dependencies" $ do
        let inputRequirements =
              [r|
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <build>
    <plugins>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-surefire-plugin</artifactId>
      </plugin>
      <plugin>
        <groupId>org.apache.maven.plugins</groupId>
        <artifactId>maven-failsafe-plugin</artifactId>
      </plugin>
      <plugin>
        <groupId>gracefully</groupId>
      </plugin>
      <plugin>
        <artifactId>ignored</artifactId>
      </plugin>
    </plugins>
  </build>
</project>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.apache.maven.plugins/maven-surefire-plugin") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "org.apache.maven.plugins/maven-failsafe-plugin") Nothing
                  ]

        result `shouldBe` expected

      it "ignores deps with missing or empty details" $ do
        let inputRequirements =
              [r|
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <dependencies>
    <!-- AppEngine runtime -->
    <dependency>
      <groupId>com.google.appengine</groupId>
    </dependency>
    <dependency>
      <artifactId>bootstrap</artifactId>
    </dependency>
    <dependency>
      <groupId>com.google.appengine</groupId>
      <artifactId></artifactId>
    </dependency>
    <dependency>
      <groupId></groupId>
      <artifactId>bootstrap</artifactId>
    </dependency>
  </dependencies>
</project>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "works with no dependencies" $ do
        let inputRequirements =
              [r|
<project xmlns="http://maven.apache.org/POM/4.0.0">
</project>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "works with random xml" $ do
        let inputRequirements =
              [r|
<Wah/>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right V.empty

        result `shouldBe` expected

      it "invalid xml produces a left" $ do
        let inputRequirements =
              [r|
askdfhaskdjfh as kdjfh ><
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input

        isLeft result `shouldBe` True

      it "gracefully ignores CDATA" $ do
        let inputRequirements =
              [r|
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>
	<groupId>com.aironman</groupId>
	<artifactId>mycxf-mongodb-spring-data-mysql-sample</artifactId>
	<version>0.0.1-SNAPSHOT</version>
	<packaging>war</packaging>

	<properties>
		<jdbc.url>
            <![CDATA[jdbc:mysql://${jdbc.database.server}/${jdbc.database.name}?autoReconnect=true&amp;createDatabaseIfNotExist=true&amp;useUnicode=true&amp;characterEncoding=utf-8]]>
		</jdbc.url>
	</properties>
</project>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected = Right V.empty

        result `shouldBe` expected

      it "loads dependencies from build profiles" $ do
        let inputRequirements =
              [r|
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <profiles>
    <profile>
        <id>debug</id>
        <dependencies>
          <dependency>
              <groupId>jstl1</groupId>
              <artifactId>jstl1</artifactId>
          </dependency>
          <dependency>
              <groupId>jstl2</groupId>
              <artifactId>jstl2</artifactId>
          </dependency>
        </dependencies>
    </profile>
    <profile>
        <id>release</id>
        <dependencies>
          <dependency>
              <groupId>jstl3</groupId>
              <artifactId>jstl3</artifactId>
          </dependency>
          <dependency>
              <groupId>jstl4</groupId>
              <artifactId>jstl4</artifactId>
          </dependency>
        </dependencies>
    </profile>
  </profiles>
</project>
|]
            input = PomXmlInput inputRequirements
            result = determineDependencies (GitPath "path") input
            expected =
              Right $
                V.fromList
                  [ BasicDependency Java (DependencyIdentifierNamed $ DependencyName "jstl1/jstl1") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "jstl2/jstl2") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "jstl3/jstl3") Nothing,
                    BasicDependency Java (DependencyIdentifierNamed $ DependencyName "jstl4/jstl4") Nothing
                  ]

        result `shouldBe` expected
