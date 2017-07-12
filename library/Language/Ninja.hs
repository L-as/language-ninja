-- -*- coding: utf-8; mode: haskell; -*-

-- File: library/Language/Ninja.hs
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

-- |
--   Module      : Language.Ninja
--   Copyright   : Copyright 2017 Awake Security
--   License     : Apache-2.0
--   Maintainer  : opensource@awakesecurity.com
--   Stability   : experimental
--
--   Tools for parsing, pretty-printing, and compiling the Ninja build language.
--
--   The module re-exports some of the modules under the @Language.Ninja@
--   namespace.
--
--   @since 0.1.0
module Language.Ninja
  ( module Language.Ninja.Lexer
  , module Language.Ninja.Parser
  , module Language.Ninja.Pretty
  ) where

import           Language.Ninja.Lexer
import           Language.Ninja.Parser
import           Language.Ninja.Pretty
