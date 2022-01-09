-- -*- coding: utf-8; mode: haskell; -*-

-- File: tests/Tests.hs
--
-- License:
--     Copyright 2017 Awake Security
--
--     Licensed under the Apache License, Version 2.0 (the "License");
--     you may not use this file except in compliance with the License.
--     You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--     Unless required by applicable law or agreed to in writing, software
--     distributed under the License is distributed on an "AS IS" BASIS,
--     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--     See the License for the specific language governing permissions and
--     limitations under the License.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The @language-ninja@ test suite.
module Main (main) where

import           Data.Maybe                 (catMaybes)
import           Data.Monoid                ((<>))

import qualified Data.Typeable              as Ty

import           Data.HashMap.Strict        (HashMap)
import           Data.HashSet               (HashSet)
import           Data.Text                  (Text)

import           Control.Exception          (displayException)
import           Control.Monad              (forM, unless, void)
import           Control.Monad.Identity     (Identity)
import           Control.Monad.Trans.Except (ExceptT, runExceptT)

import qualified Control.Lens               as Lens

import qualified Language.Ninja.AST.Env     as AST (Maps)

import qualified Language.Ninja.AST         as AST
import qualified Language.Ninja.Compile     as Compile
import qualified Language.Ninja.Errors      as Errors
import qualified Language.Ninja.IR          as IR
import qualified Language.Ninja.Lexer       as Lexer
import qualified Language.Ninja.Misc        as Misc
import qualified Language.Ninja.Parser      as Parser
import qualified Language.Ninja.Pretty      as Pretty

import           Test.Tasty                 (TestName, TestTree)

import qualified Test.Tasty                 as Test
import qualified Test.Tasty.HUnit           as Test
import qualified Test.Tasty.Ingredients     as Test
import qualified Test.Tasty.Options         as Test
import qualified Test.Tasty.Runners.Html    as Test

import qualified Test.Tasty.QuickCheck      as Test.QC
import qualified Test.Tasty.SmallCheck      as Test.SC

import           Test.QuickCheck            ((===))
import qualified Test.QuickCheck            as QC
import           Test.QuickCheck.Instances  ()

import qualified Test.SmallCheck.Series     as SC

import           Filesystem.Path.CurrentOS  ((</>))
import qualified Filesystem.Path.CurrentOS  as FP

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson

import qualified Turtle

import           Flow                       ((.>), (|>))

import           Tests.Orphans              ()
import qualified Tests.ReferenceLexer       as RefLex

--------------------------------------------------------------------------------

dataPrefix :: String
dataPrefix = "./tests/data/"

testFiles :: [String]
testFiles = [ "buildseparate"
            , "compdb"
            , "lexical"
            , "lint"
            , "nocreate"
            , "outputtouch"
            , "phonyorder"
            , "redefine"
            , "test1"
            , "test2"
            , "test3"
            , "test4"
            , "test5"
            , "test6"
            ]

aesonSC' :: (Eq x, Show x)
         => Ty.Proxy x
         -> SC.Series IO x
         -> (x -> Aeson.Value)
         -> (Aeson.Value -> Aeson.Parser x)
         -> TestTree
aesonSC' _ s toJ fromJ
  = Test.SC.testProperty "parseJSON . toJSON ≡ₛ pure"
    (Test.SC.over s (\x -> Aeson.parseEither fromJ (toJ x) == Right x))

aesonSC :: forall x.
           ( Eq x, Show x, SC.Serial IO x, Aeson.ToJSON x, Aeson.FromJSON x
           ) => Ty.Proxy x -> TestTree
aesonSC _ = aesonSC' (Ty.Proxy :: Ty.Proxy x)
            SC.series Aeson.toJSON Aeson.parseJSON

aesonQC' :: (Eq x, Show x)
         => Ty.Proxy x
         -> (QC.Gen x, x -> [x])
         -> (x -> Aeson.Value)
         -> (Aeson.Value -> Aeson.Parser x)
         -> TestTree
aesonQC' _ (gen, shrink) toJ fromJ
  = Test.QC.testProperty "parseJSON . toJSON ≡ₐ pure"
    (Test.QC.forAllShrink gen shrink
     (\x -> Aeson.parseEither fromJ (toJ x) === Right x))

aesonQC :: forall x.
           ( Eq x, Show x, QC.Arbitrary x, Aeson.ToJSON x, Aeson.FromJSON x
           ) => Ty.Proxy x -> TestTree
aesonQC _ = aesonQC' (Ty.Proxy :: Ty.Proxy x)
            (QC.arbitrary, QC.shrink) Aeson.toJSON Aeson.parseJSON

parseTestNinja :: String -> IO (AST.Ninja ())
parseTestNinja name = do
  old <- Turtle.pwd
  Turtle.cd (FP.decodeString dataPrefix)
  let file = Lens.view (Lens.from Misc.pathString) (name <> ".ninja")
  result <- Parser.parseFileIO file >>= void .> pure
  Turtle.cd old
  pure result

lexerEquivalentTest :: String -> IO ()
lexerEquivalentTest name = do
  let file = dataPrefix <> name <> ".ninja"
             |> Lens.view (Lens.from Misc.pathString)

  let oldLexer :: Misc.Path -> ExceptT Errors.ParseError IO [Lexer.Lexeme ()]
      oldLexer = RefLex.lexFile

  let newLexer :: Misc.Path -> ExceptT Errors.ParseError IO [Lexer.Lexeme ()]
      newLexer = Lexer.lexFile .> fmap (map void)

  expected <- runExceptT (oldLexer file)
  actual   <- runExceptT (newLexer file)

  unless (expected == actual) $ do
    Test.assertEqual "prefix" expected actual

roundtripTest :: AST.Ninja () -> IO ()
roundtripTest ninja = do
  let withTempDir :: (FP.FilePath -> IO a) -> IO a
      withTempDir = Turtle.with (Turtle.mktempdir "." "test")

  (expected, actual) <- withTempDir $ \tmpdir -> do
    let prettyInput = Pretty.prettyNinja ninja
    let tmpfile = tmpdir </> "generated.ninja"
    Turtle.writeTextFile tmpfile prettyInput
    output <- Parser.parseFileIO (Lens.view (Lens.from Misc.pathFP) tmpfile)
              >>= void .> pure
    let prettyOutput = Pretty.prettyNinja output
    pure (prettyInput, prettyOutput)

  unless (expected == actual) $ do
    -- let actualJ   = Aeson.toJSON actual
    -- let expectedJ = Aeson.toJSON expected
    -- -- LBSC8.putStrLn (Aeson.encodePretty (Aeson.diff actualJ expectedJ))
    -- LBSC8.putStrLn (Aeson.encodePretty expectedJ)
    -- LBSC8.putStrLn (Aeson.encodePretty actualJ)
    -- Aeson.encode actualJ `H.shouldBe` Aeson.encode expectedJ
    Test.assertEqual "prefix" expected actual

compileTest :: AST.Ninja () -> IO ()
compileTest ninja = void $ do
  either (displayException .> fail) pure (Compile.compile ninja)

ninjaTests :: String -> AST.Ninja () -> TestTree
ninjaTests name ninja
  = Test.testGroup (name <> ".ninja")
    [ Test.testCase "compare lexer against reference implementation" $ do
        lexerEquivalentTest name
    , Test.testCase "roundtrip through parser and pretty-printer" $ do
        roundtripTest ninja
    , Test.testCase "compile to Ninja" $ do
        compileTest ninja
    ]

aesonTests :: TestTree
aesonTests
  = Test.testGroup "aeson"
    [ testModule "Language.Ninja.Lexer"
      [ testAesonSC 2   (Ty.Proxy :: Ty.Proxy (Lexer.Lexeme Bool))
      , testAesonSC 4   (Ty.Proxy :: Ty.Proxy (Lexer.LName  Bool))
      , testAesonSC 4   (Ty.Proxy :: Ty.Proxy (Lexer.LFile  Bool))
      , testAesonSC 4   (Ty.Proxy :: Ty.Proxy (Lexer.LBind  Bool))
      , testAesonSC 2   (Ty.Proxy :: Ty.Proxy (Lexer.LBuild Bool))
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Build"
      [ testAesonSC 2   (Ty.Proxy :: Ty.Proxy IR.Build)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Meta"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy IR.Meta)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Ninja"
      [ testAesonSC 2   (Ty.Proxy :: Ty.Proxy IR.Ninja)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Pool"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy IR.Pool)
      , testAesonSC def (Ty.Proxy :: Ty.Proxy IR.PoolName)
      , testAesonSC def (Ty.Proxy :: Ty.Proxy IR.PoolDepth)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Rule"
      [ testAesonSC 1   (Ty.Proxy :: Ty.Proxy IR.Rule)
      , testAesonSC def (Ty.Proxy :: Ty.Proxy IR.SpecialDeps)
      , testAesonSC def (Ty.Proxy :: Ty.Proxy IR.ResponseFile)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.IR.Target"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy IR.Target)
      , testAesonSC def (Ty.Proxy :: Ty.Proxy IR.Output)
      , testAesonSC def (Ty.Proxy :: Ty.Proxy IR.Dependency)
      , testAesonSC def (Ty.Proxy :: Ty.Proxy IR.DependencyType)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.AST.Env"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy (AST.Env Text Text))
      , testAesonQC     (Ty.Proxy :: Ty.Proxy (AST.Env Text Text))
      ]
    , testModule "Language.Ninja.AST.Expr"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy (AST.Expr Bool))
      , testAesonQC     (Ty.Proxy :: Ty.Proxy (AST.Expr Bool))
      ]
    , testModule "Language.Ninja.AST.Rule"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy (AST.Rule Bool))
      , testAesonQC     (Ty.Proxy :: Ty.Proxy (AST.Rule Bool))
      ]
    , testModule "Language.Ninja.AST.Ninja"
      [ -- TODO: combinatorial explosion
        testAesonSC 0   (Ty.Proxy :: Ty.Proxy (AST.Ninja Bool))
      -- , testAesonQC     (Ty.Proxy :: Ty.Proxy (AST.Ninja Bool))
      ]
    , testModule "Language.Ninja.AST.Build"
      [ -- TODO: combinatorial explosion
        testAesonSC 1   (Ty.Proxy :: Ty.Proxy (AST.Build Bool))
      -- , testAesonQC     (Ty.Proxy :: Ty.Proxy (AST.Build Bool))
      ]
    , testModule "Language.Ninja.AST.Deps"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy (AST.Deps Bool))
      , testAesonQC     (Ty.Proxy :: Ty.Proxy (AST.Deps Bool))
      ]
    , testModule "Language.Ninja.Misc.Command"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy Misc.Command)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.Misc.Path"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy Misc.Path)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.Misc.Located"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy (Misc.Located Bool))
      , testAesonSC def (Ty.Proxy :: Ty.Proxy Misc.Position)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    , testModule "Language.Ninja.Misc.IText"
      [ testAesonSC def (Ty.Proxy :: Ty.Proxy Misc.IText)
      -- TODO: add Arbitrary instances so we can testAesonQC
      ]
    ]
  where
    testAesonSC :: forall x.
                   ( Eq x, Show x, Ty.Typeable x, SC.Serial IO x
                   , Aeson.ToJSON x, Aeson.FromJSON x
                   ) => Int -> Ty.Proxy x -> Maybe TestTree
    testAesonSC d p = withDepth d
                      (testType p
                       [Test.testGroup "ToJSON/FromJSON Laws" [aesonSC p]])

    testAesonQC :: forall x.
                   ( Eq x, Show x, Ty.Typeable x, QC.Arbitrary x
                   , Aeson.ToJSON x, Aeson.FromJSON x
                   ) => Ty.Proxy x -> Maybe TestTree
    testAesonQC p = Just (testType p
                          [Test.testGroup "ToJSON/FromJSON Laws" [aesonQC p]])

    withDepth :: Int -> (TestTree -> Maybe TestTree)
    withDepth 0     = const Nothing
    withDepth depth = Test.localOption (Test.SC.SmallCheckDepth depth) .> Just

    testModule :: TestName -> [Maybe TestTree] -> TestTree
    testModule name subtrees = Test.testGroup name (catMaybes subtrees)

    testType :: forall x. (Ty.Typeable x)
             => Ty.Proxy x -> [TestTree] -> TestTree
    testType p subtrees = Test.localOption typeTimeout
                          $ Test.testGroup (printProxy p) subtrees

    printProxy :: forall x. (Ty.Typeable x) => Ty.Proxy x -> String
    printProxy p = Ty.showsTypeRep (Ty.typeRep p) ""

    def :: Int
    def = (Test.defaultValue :: Test.SC.SmallCheckDepth)
          |> toInteger |> fromIntegral

    typeTimeout :: Test.Timeout
    typeTimeout = Test.mkTimeout 20000000 -- 20 seconds

ingredients :: IO [Test.Ingredient]
ingredients = [ [Test.htmlRunner]
              , Test.defaultIngredients
              ] |> mconcat |> pure

testTree :: IO TestTree
testTree = do
  ninjas <- forM testFiles parseTestNinja

  let tests = Test.testGroup "language-ninja"
              [ Test.testGroup "golden"
                 (fmap (uncurry ninjaTests) (zip testFiles ninjas))
              , aesonTests
              ]
  pure tests

test :: IO ()
test = do
  is <- ingredients
  tree <- testTree
  Test.defaultMainWithIngredients is tree

main :: IO ()
main = do
  test

--------------------------------------------------------------------------------
