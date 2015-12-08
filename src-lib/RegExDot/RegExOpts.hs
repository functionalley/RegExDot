{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-
	Copyright (C) 2010-2015 Dr. Alistair Ward

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]

	* Provides a complete job-specification.

	* 'regEx' is polymorphic, allowing use of either /RegExDot.RegEx.ExtendedRegEx/ (which is also polymorphic) or /RegExChar.ExtendedRegExChar.ExtendedRegExChar/.
-}

module RegExDot.RegExOpts(
-- * Types
-- ** Data-types
	RegExOpts(..),
-- * Functions
	setVerbose,
-- ** Constructors
	mkRegEx
) where

import qualified	Data.Default
import qualified	RegExDot.CompilationOptions	as CompilationOptions
import qualified	RegExDot.ExecutionOptions	as ExecutionOptions

-- | Aggregates both 'ExecutionOptions.ExecutionOptions' & 'CompilationOptions.CompilationOptions' with a polymorphic /regex/, to form a complete job-description.
data RegExOpts a	= MkRegExOpts {
	compilationOptions	:: CompilationOptions.CompilationOptions,	-- ^ Parameters governing the result, rather than the implementation.
	executionOptions	:: ExecutionOptions.ExecutionOptions,		-- ^ Parameters governing the implementation, rather than the result.
	regEx			:: a						-- ^ Polymorphic, to permit specialisation either of the type of /regex/ or the type of input-data.
} deriving Show

instance Functor RegExOpts	where
	fmap f regExOpts	= regExOpts { regEx = f $ regEx regExOpts }

-- | Smart constructor.
mkRegEx :: a -> RegExOpts a
mkRegEx	r	= MkRegExOpts {
	compilationOptions	= Data.Default.def,
	executionOptions	= Data.Default.def,
	regEx			= r
}

-- | Mutator, which sets an appropriate 'ExecutionOptions.ExecutionOptions', for the depth to which the caller wants to probe the resulting match.
setVerbose :: Bool -> RegExOpts a -> RegExOpts a
setVerbose verbose regExOpts	= regExOpts { executionOptions = ExecutionOptions.setVerbose verbose $ executionOptions regExOpts }

