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

	* Defines /binary/ operators, to form a /Domain-specific Language/, by which to compose 'RegEx.Concatenations' from 'RegEx.Pattern's.

	* Each operator quantifies the specified 'RegEx.Pattern', & prepends it to the specified 'RegEx.Concatenation'.

 [@TODO@]	Could @[]@ & 'Meta' be instances of a type-class which has methods (-:) etc.,
		allowing the former to prepend 'RegEx.captureGroup' & the latter 'RegEx.Require' ?

-}

module RegExDot.DSL(
-- * Functions
-- ** Operators
	(-:),
	(?:),
	(??:),
	(*:),
	(*?:),
	(+:),
	(+?:),
	( #->#:),
	( #->#?:),
	( #->:),
	( #->?:),
	( #:),
	(<~>)
) where

import			RegExDot.Repeatable((^#->#), (^#->#?), (^#->), (^#->?), (^#))
import qualified	RegExDot.Anchor		as Anchor
import qualified	RegExDot.RegEx		as RegEx
import qualified	RegExDot.Repeatable	as Repeatable

infixr 5 -:, ?:, ??:, *:, *?:, +:, +?:,#->#:, #->#?:, #->:, #->?:, #:, <~>	-- Same as for ':', & lower than Repeatable's operators.

-- | Prepend an unrepeated 'RegEx.Pattern', to the specified 'RegEx.Concatenation'.
(-:) :: RegEx.Pattern a -> RegEx.Concatenation a -> RegEx.Concatenation a
(-:) pattern	= (Repeatable.one pattern :)

-- | Prepend an optional 'RegEx.Pattern', to the specified 'RegEx.Concatenation'.
(?:) :: RegEx.Pattern a -> RegEx.Concatenation a -> RegEx.Concatenation a
(?:) pattern	= (Repeatable.zeroOrOne pattern :)

-- | A /non-greedy/ version of '?:'.
(??:) :: RegEx.Pattern a -> RegEx.Concatenation a -> RegEx.Concatenation a
(??:) pattern	= (Repeatable.zeroOrOne' pattern :)

-- | Prepend a 'RegEx.Pattern', repeatable zero or more times, to the specified 'RegEx.Concatenation'.
(*:) :: RegEx.Pattern a -> RegEx.Concatenation a -> RegEx.Concatenation a
(*:) pattern	= (Repeatable.zeroOrMore pattern :)

-- | A /non-greedy/ version of '*:'.
(*?:) :: RegEx.Pattern a -> RegEx.Concatenation a -> RegEx.Concatenation a
(*?:) pattern	= (Repeatable.zeroOrMore' pattern :)

-- | Prepend a 'RegEx.Pattern', repeatable one or more times, to the specified 'RegEx.Concatenation'.
(+:) :: RegEx.Pattern a -> RegEx.Concatenation a -> RegEx.Concatenation a
(+:) pattern	= (Repeatable.oneOrMore pattern :)

-- | A /non-greedy/ version of '+:'.
(+?:) :: RegEx.Pattern a -> RegEx.Concatenation a -> RegEx.Concatenation a
(+?:) pattern	= (Repeatable.oneOrMore' pattern :)

-- | Prepend a 'RegEx.Pattern', repeated a range of times, to the specified 'RegEx.Concatenation'.
( #->#:) :: (RegEx.Pattern a, Repeatable.RepetitionBounds) -> RegEx.Concatenation a -> RegEx.Concatenation a
( #->#:) (pattern, bounds)	= (pattern ^#-># bounds :)

-- | A /non-greedy/ version of '#->#:'.
( #->#?:) :: (RegEx.Pattern a, Repeatable.RepetitionBounds) -> RegEx.Concatenation a -> RegEx.Concatenation a
( #->#?:) (pattern, bounds)	= (pattern ^#->#? bounds :)

-- | Prepend a 'RegEx.Pattern', repeated at least a specified number of times, to the specified 'RegEx.Concatenation'.
( #->:) :: (RegEx.Pattern a, Repeatable.Repetitions) -> RegEx.Concatenation a -> RegEx.Concatenation a
( #->:) (pattern, fewest)	= (pattern ^#-> fewest :)

-- | A /non-greedy/ version of '#->:'.
( #->?:) :: (RegEx.Pattern a, Repeatable.Repetitions) -> RegEx.Concatenation a -> RegEx.Concatenation a
( #->?:) (pattern, fewest)	= (pattern ^#->? fewest :)

-- | Prepend a 'RegEx.Pattern', repeated a precise number of times, to the specified 'RegEx.Concatenation'.
( #:) :: (RegEx.Pattern a, Repeatable.Repetitions) -> RegEx.Concatenation a -> RegEx.Concatenation a
( #:) (pattern, r)	= (pattern ^# r :)

{- |
	* Sandwiches a 'RegEx.Concatenation' between optional 'Anchor.Anchor's to construct a 'RegEx.ExtendedRegEx'.

	* Accounts for the unusual requirement to place an 'Anchor.Stern' at the start of the 'RegEx.Concatenation', or an 'Anchor.Bow' at the end.
-}
(<~>)
	:: Anchor.AnchorSpecification	-- ^ A specification for the anchors required around the expression.
	-> RegEx.Concatenation a	-- ^ The bare list of repeatable patterns to anchor.
	-> RegEx.ExtendedRegEx a	-- ^ The completed regex.
(bow, stern) <~> concatenation = RegEx.MkExtendedRegEx {
	RegEx.bowAnchor		= bow,
	RegEx.concatenation	= concatenation,
	RegEx.sternAnchor	= stern
}
