{-
	Copyright (C) 2010 Dr. Alistair Ward

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

 [@DESCRIPTION@]	Tools to manipulate a 'RegEx.Result'.

 [@CAVEAT@]	The data-definition remains in "RegEx", since it references 'RegEx.Match'.
-}

module RegExDot.Result(
-- * Functions
-- ** Accessors
	getMatchList,
	getPreMatch,
	getPostMatch,
-- ** Predicates
	isMatch,
-- ** Query
	countMatches
) where

import qualified	Data.Maybe
import qualified	RegExDot.RegEx	as RegEx

-- | Accessor.
getPreMatch :: RegEx.Result a -> RegEx.ExternalMatch a
getPreMatch (externalMatch, _, _)	= externalMatch

-- | Accessor.
getPostMatch :: RegEx.Result a -> RegEx.ExternalMatch a
getPostMatch (_, _, externalMatch)	= externalMatch

-- | Accessor.
getMatchList :: RegEx.Result a -> Maybe (RegEx.MatchList a)
getMatchList (_, maybeMatchList, _)	= maybeMatchList

-- | True if the 'RegEx.InputData' matched the 'RegEx.ExtendedRegEx'.
isMatch :: RegEx.Result a -> Bool
isMatch	= Data.Maybe.isJust . getMatchList

-- | Counts the number of top-level 'RegEx.Match'es.
countMatches :: RegEx.Result a -> Int
countMatches (_, Just matchList, _)	= length matchList
countMatches _				= 0

