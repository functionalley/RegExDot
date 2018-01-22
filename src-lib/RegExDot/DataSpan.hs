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
	along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{- |
 [@AUTHOR@]	Dr. Alistair Ward

 [@DESCRIPTION@]	Augments a 'Span.Span', with the specific data concerned.
-}

module RegExDot.DataSpan(
-- * Types
-- ** Type-synonyms
	DataSpan,
-- * Functions
	after,
	empty,
	join
) where

import			Control.Arrow((***))
import qualified	RegExDot.ConsumptionBounds	as ConsumptionBounds
import qualified	RegExDot.Span			as Span

{- |
	* Augment 'Span.Span' with a copy of the data to which it refers.

	* Similar to <https://hackage.haskell.org/packages/archive/regex-base/latest/doc/html/Text-Regex-Base-RegexLike.html#t%3AMatchText>.
-}
type DataSpan a	= ([a], Span.Span)

-- | Constructs an empty instance, located at the specified offset.
empty :: ConsumptionBounds.DataLength -> DataSpan a
empty offset	= ([], Span.empty offset)

-- | Returns the furthest extent, i.e. the offset just after the end.
after :: DataSpan a -> ConsumptionBounds.DataLength
after	= Span.after . snd

{- |
	* Condenses a list into a single value.

	* Expects a gap-free, sorted list.
-}
join
	:: ConsumptionBounds.DataLength	-- ^ The offset at which the concatenated match is considered to have occurred.
	-> [DataSpan a]			-- ^ The list to be concatenated.
	-> DataSpan a
join offset	= (concat *** Span.join offset) . unzip
