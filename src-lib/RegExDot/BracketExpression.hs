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

 [@DESCRIPTION@]

	This concept was traditionally known as a /Character-class/,
	but to distinguish it from the more recently introduced & narrower /POSIX Character-class/, it has been renamed /Bracket-expression/.
-}

module RegExDot.BracketExpression(
-- * Types
-- ** Type-synonyms
	BracketExpression,
-- * Constants
	delimiterTokens,
	negationToken,
	tokens,
-- * Functions
-- ** Predicates
	containsMatch
) where

import			RegExDot.BracketExpressionMember((=~))
import qualified	RegExDot.BracketExpressionMember	as BracketExpressionMember

-- | A /Bracket-expression/ consists of a list of 'BracketExpressionMember.Member's.
type BracketExpression m	= [BracketExpressionMember.Member m]

-- | True if the specified datum matches any 'BracketExpressionMember.Member' of the 'BracketExpression'.
containsMatch :: Eq m
	=> m			-- ^ The input datum.
	-> BracketExpression m	-- ^ The list of bracket-expression members within which to find a match.
	-> Bool
containsMatch datum	= any (datum =~)

-- | The delimiters of a /bracket-expression/, when in 'String'-form.
delimiterTokens :: (Char, Char)
delimiterTokens	= ('[', ']')

-- | Used to denote a negated /bracket-expression/, when in 'String'-form.
negationToken :: Char
negationToken	= '^'

-- | The set of 'Char' to which a specific meaning is attributed, when reading from 'String'.
tokens :: String
tokens	= [fst delimiterTokens, snd delimiterTokens, negationToken]

