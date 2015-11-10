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

 [@DESCRIPTION@]	Describes the bounds of data-consumption.
-}

module RegExDot.ConsumptionBounds(
-- * Types
-- ** Type-synonyms
	ConsumptionBounds,
	DataLength,
-- * Constants
	zero,
-- * Functions
-- ** Predicates
	isPrecise
) where

-- | A measure of the quantity of an unspecified type of data.
type DataLength	= Int

-- | The minimum & maximum bounds of potential data-consumption.
type ConsumptionBounds	= (DataLength, Maybe DataLength)

-- | Exactly zero consumption.
zero :: ConsumptionBounds
zero	= (0, Just 0)

-- | Predicate which is 'True' if only a precise quantity of data is consumable; no more, no less.
isPrecise :: ConsumptionBounds -> Bool
isPrecise (fewest, most)	= Just fewest == most

