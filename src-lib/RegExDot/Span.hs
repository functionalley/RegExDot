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

 [@DESCRIPTION@]	Delimits a data-sequence, via its offset & length, within a wider list.
-}

module RegExDot.Span(
-- * Types
-- ** Type-synonyms
	Span,
-- * Functions
	after,
	empty,
	join
) where

import			Control.Arrow((***))
import qualified	RegExDot.ConsumptionBounds	as ConsumptionBounds

{- |
	* The offset & length of a specific data-sequence, within a wider list.

	* Similar to <https://hackage.haskell.org/packages/archive/regex-base/latest/doc/html/Text-Regex-Base-RegexLike.html#t%3AMatchArray>.
-}
type Span	= (ConsumptionBounds.DataLength {-offset-}, ConsumptionBounds.DataLength)

-- | An empty instance, located at the specified offset.
empty :: ConsumptionBounds.DataLength -> Span
empty offset	= (offset, 0)

-- | Returns the furthest extent, i.e. the offset just after the end.
after :: Span -> ConsumptionBounds.DataLength
after	= uncurry (+)

{- |
	* Condenses a list into a single value.

	* Expects a gap-free, sorted list.
-}
join
	:: ConsumptionBounds.DataLength	-- ^ The offset into the list of input-data to use when a null list of spans is received.
	-> [Span]
	-> Span
join offset []		= empty offset		-- The offset can't be deduced from a null list, so use the value provided.
join _ [singleton]	= singleton		-- Merely for efficiency.
join _ spanList		= head *** sum $ unzip spanList


