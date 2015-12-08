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

	* Defines those options relating to compile-time.

	* These tend to be options which affect the result, rather than the means by which it is achieved.

	* Whilst similar structures are present in other regex-implementations, there's no standardisation of the fields.
-}

module RegExDot.CompilationOptions(
-- * Types
-- ** Data-types
	CompilationOptions(..)
) where

import qualified	Data.Default
import qualified	ToolShed.Options

-- | The switch(es) used to control compilation of the /regex/-engine.
data CompilationOptions	= MkCompilationOptions {
	complyStrictlyWithPosix	:: Bool		-- ^ Define the offset of captured data, corresponding to a sub-expression which matched zero times, as the artificial value @-1@ specified by POSIX.
} deriving (Eq, Show)

instance Data.Default.Default CompilationOptions	where
	def	= ToolShed.Options.blankValue { complyStrictlyWithPosix	= True }

instance ToolShed.Options.Options CompilationOptions	where
	blankValue	= MkCompilationOptions { complyStrictlyWithPosix = undefined }

