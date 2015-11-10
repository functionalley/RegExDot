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

	* A type of /zero-width assertion/:
	the 'Bow'-predicate is 'True' if no input-data has yet been consumed; the 'Stern'-predicate is 'True' if no input-data remains to be consumed.

	* Whilst at the top-level of the regex, the /absence/ of an anchor is equivalent to a non-capturing @.*?@,
	tempting one to implement them using this property,
	it doesn't apply to those anchors encountered at either end of a sub-expression.
	So in all cases, an anchor constrains the permissible consumption of input-data,
	but at the top level it also prevents the regex drifting away from either end of the input-data.
-}

module RegExDot.Anchor(
-- * Types
-- ** Data-types
	Anchor(..),
-- ** Type-synonyms
	AnchorSpecification,
-- * Constants
	bowToken,
	sternToken,
	tokens,
	unanchored
) where

import qualified	Control.DeepSeq

-- | Defines the types on /anchor/ by which a /regex/ can be moored to a part of the input-data.
data Anchor =
	Bow	-- ^ Matches only if no input data has yet been consumed. Can only exist at the start of the entire regex, or (in theory) the start of any /alternative/.
	| Stern	-- ^ Matches only if no input data remains to be consumed. Can only exist at the end of the entire regex, or (in theory) the end of any /alternative/.
	deriving (
		Eq
--		Read,	-- See specialisation below.
--		Show	-- See specialisation below.
	)

instance Show Anchor	where
	showsPrec _ Bow		= showChar bowToken
	showsPrec _ Stern	= showChar sternToken

instance Read Anchor	where
	readsPrec _ []		= []	-- No parse.
	readsPrec _ (' ' : s)	= reads s	-- Consume white-space.
	readsPrec _ ('\t' : s)	= reads s	-- Consume white-space.
	readsPrec _ (c : s)	= case c `lookup` [(bowToken, Bow), (sternToken, Stern)] of
		Just anchor	-> [(anchor, s)]
		_		-> []	-- No parse.

instance Control.DeepSeq.NFData Anchor	where
	rnf _	= ()

-- | The conventional token used to denote a 'Bow'-anchor, when in 'String'-form.
bowToken :: Char
bowToken	= '^'

-- | The conventional token used to denote a 'Stern'-anchor, when in 'String'-form.
sternToken :: Char
sternToken	= '$'

-- | The set of 'Char' to which a specific meaning is attributed, when reading from 'String'.
tokens :: String
tokens	= [bowToken, sternToken]

-- | A specification for the anchors required around the expression
type AnchorSpecification	= (Maybe Anchor, Maybe Anchor)

-- | The specification for an unanchored expression.
unanchored :: AnchorSpecification
unanchored	= (Nothing, Nothing)

