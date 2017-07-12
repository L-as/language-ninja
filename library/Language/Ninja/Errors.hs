-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja/Errors.hs
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

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
--   Module      : Language.Ninja.Errors
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   The module re-exports the modules under @Language.Ninja.Errors.*@, all of
--   which are related to error types used in @language-ninja@.
--   It also defines the 'NinjaError' type.
--
--   @since 0.1.0
module Language.Ninja.Errors
  ( module Language.Ninja.Errors.Compile
  , module Language.Ninja.Errors.Parser
  , NinjaError (..)
  , throwNinjaError
  , throwGenericNinjaError
  , throwNinjaParseError
  , throwNinjaCompileError
  ) where

import           Language.Ninja.Errors.Compile
import           Language.Ninja.Errors.Parser

import           Control.Exception             (Exception)
import           Control.Monad.Error.Class     (MonadError (..))
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)

--------------------------------------------------------------------------------

-- | This type subsumes any error that can be thrown during execution of a
--   function defined in @language-ninja@.
--
--   @since 0.1.0
data NinjaError
  = -- | Generic catch-all error constructor. Avoid using this.
    --
    --   @since 0.1.0
    GenericNinjaError !Text
  | -- | Errors encountered during parsing.
    --
    --   @since 0.1.0
    NinjaParseError   !ParseError
  | -- | Errors encountered during compilation.
    --
    --   @since 0.1.0
    NinjaCompileError !CompileError
  deriving (Eq, Show, Generic)

instance Exception NinjaError

-- | Throw a 'NinjaError'.
--
--   @since 0.1.0
throwNinjaError :: (MonadError NinjaError m) => NinjaError -> m a
throwNinjaError = throwError

-- | Throw a generic catch-all 'NinjaError'.
--
--   @since 0.1.0
throwGenericNinjaError :: (MonadError NinjaError m) => Text -> m a
throwGenericNinjaError msg = throwNinjaError (GenericNinjaError msg)

-- | Throw a 'ParseError'.
--
--   @since 0.1.0
throwNinjaParseError :: (MonadError NinjaError m) => ParseError -> m a
throwNinjaParseError e = throwNinjaError (NinjaParseError e)

-- | Throw a 'CompileError'.
--
--   @since 0.1.0
throwNinjaCompileError :: (MonadError NinjaError m) => CompileError -> m a
throwNinjaCompileError e = throwNinjaError (NinjaCompileError e)

--------------------------------------------------------------------------------
