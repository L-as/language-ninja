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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--   Module      : Main
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   FIXME: doc
module Tests.Orphans
  ( -- No exports other than instances.
  ) where

import           Data.Hashable          (Hashable)

import qualified Data.HashMap.Strict    as HM
import qualified Data.HashSet           as HS
import qualified Data.List.NonEmpty     as NE
import qualified Data.Text              as T
import qualified Data.Versions          as V

import           Test.SmallCheck.Series ((<~>), (>>-), (\/))
import qualified Test.SmallCheck.Series as SC

import           Flow                   ((.>), (|>))

--------------------------------------------------------------------------------

instance ( Monad m, SC.Serial m k, SC.Serial m v, Eq k, Hashable k
         ) => SC.Serial m (HM.HashMap k v) where
  series = pure HM.empty
           \/ (HM.singleton <$> SC.series <~> SC.series)
           -- \/ (HM.union <$> SC.series <~> SC.series)

instance ( Monad m, SC.CoSerial m k, SC.CoSerial m v, Eq k, Hashable k
         ) => SC.CoSerial m (HM.HashMap k v) where
  coseries = SC.coseries .> fmap (\f -> HM.toList .> f)

--------------------------------------------------------------------------------

instance ( Monad m, SC.Serial m a, Eq a, Hashable a
         ) => SC.Serial m (HS.HashSet a) where
  series = pure HS.empty
           \/ (HS.singleton <$> SC.series)
           -- \/ (HS.union <$> SC.series <~> SC.series)

instance ( Monad m, SC.CoSerial m a, Eq a, Hashable a
         ) => SC.CoSerial m (HS.HashSet a) where
  coseries = SC.coseries .> fmap (\f -> HS.toList .> f)

--------------------------------------------------------------------------------

instance (Monad m, SC.Serial m a) => SC.Serial m (NE.NonEmpty a) where
  series = SC.series |> fmap (pure .> NE.fromList)
  -- series = SC.series |> fmap (SC.getNonEmpty .> NE.fromList)

instance (Monad m, SC.CoSerial m a) => SC.CoSerial m (NE.NonEmpty a) where
  coseries = SC.coseries .> fmap (\f -> NE.toList .> f)

--------------------------------------------------------------------------------

instance (Monad m) => SC.Serial m T.Text where
  series = foldr1 (\/) (map pure testText)

instance (Monad m) => SC.CoSerial m T.Text where
  coseries rs = SC.alts0 rs >>- \y -> SC.alts2 rs >>- \f -> do
    pure (T.uncons .> (\case Nothing        -> y
                             (Just (b, bs)) -> f (T.singleton b) bs))

--------------------------------------------------------------------------------

instance (Monad m) => SC.Serial m V.Version where
  series = foldr1 (\/) (map pure testVersions)

instance (Monad m, SC.CoSerial m V.VUnit) => SC.CoSerial m V.Version where
  coseries = SC.coseries
             .> fmap (\f -> \(V.Version {..}) -> f (_vEpoch, _vChunks, _vRel))

instance (Monad m) => SC.CoSerial m V.VUnit where
  coseries = SC.coseries
             .> fmap (\f -> \case (V.Digits i) -> f (Right i)
                                  (V.Str    s) -> f (Left  s))

--------------------------------------------------------------------------------

testText :: [T.Text]
testText = ["", "foo", "42", " "]

testVersions :: [V.Version]
testVersions = [ "0.1.0"
               , "0.2"
               , "0.2.0"
               , "0.2.0.0"
               , "0.25-2"
               , "0.9.9.9"
               , "1"
               , "1.0"
               , "1.0.0"
               , "1.0.0.0"
               , "1.0.0.1"
               , "1.0.0-alpha"
               , "1.0.0-alpha.1"
               , "1.0.0-alpha.beta"
               , "1.0.0-beta"
               , "1.0.0-beta.11"
               , "1.0.0-beta.2"
               , "1.0.0-rc.1"
               , "1:0.10.16-3"
               , "1.0rc0"
               , "1.0rc1"
               , "1.1"
               , "1:1.0"
               , "1:1.1"
               , "1:1.2.3-1"
               , "1.1rc1"
               , "1.2"
               , "1.2.2r1-1"
               , "1.2.3"
               , "1.2.3-1"
               , "1.2.3-alpha"
               , "1.2.3-alpha.2"
               , "1.2.3r1"
               , "1.2.3r1-1"
               , "1.2.4"
               , "1.2.4r1-1"
               , "1.2-5"
               , "1.58.0-3"
               , "2"
               , "20150826-1"
               , "21-2"
               , "2.3.4"
               , "3.4.5"
               , "44.0.2403.157-1"
               , "7.1p1-1"
               , "8.u51-1"
               ] |> map (V.version .> fromRight)
  where
    fromRight :: (Show a, Show b) => Either a b -> b
    fromRight (Left  a) = error ("fromRight: " ++ show a)
    fromRight (Right b) = b

--------------------------------------------------------------------------------

