{-# LANGUAGE CPP #-}
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

	A 'RegExDot.ConsumptionProfile' is composed from both a capacity to consume, & an ability to discriminate based on the specific data.
	The former is described by 'consumptionBounds' & the latter by both 'hasSpecificRequirement' & 'canConsumeAnything'.
-}

module RegExDot.ConsumptionProfile(
-- * Types
-- ** Type-synonyms
	AccumulatedConsumptionProfiles,
-- ** Data-types
	ConsumptionProfile(..),
-- * Constants
	zero,
-- * Functions
	accumulateFrom,
	aggregateFromConcatenation,
-- ** Operators
	(|+|),
	(<>),
-- ** Predicates
--	isPrecise,
	withinConsumptionBounds
) where

import			Control.Arrow((***))
import qualified	Data.List
import qualified	Data.Maybe
import qualified	RegExDot.ConsumptionBounds	as ConsumptionBounds
import qualified	ToolShed.SelfValidate

#if MIN_VERSION_base(4,11,0)
import	Prelude hiding ((<>))	-- N.B.: avoid ambiguity with '(Data.Semigroup.<>)'.
#endif

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

infixr 5 |+|	-- Same as (++).
infixr 2 <>	-- Same as (||).

{- |
	* A 'Consumer' is considered to have a 'ConsumptionProfile' composed from both a capacity to consume, & an ability to discriminate.

	* Whilst 'hasSpecificRequirement' & 'canConsumeAnything' look like opposites, they can assume independent values; the instance can be both, but only be neither when empty.
-}
data ConsumptionProfile	= MkConsumptionProfile {
	consumptionBounds	:: ConsumptionBounds.ConsumptionBounds,	-- ^ The permissible quantity of data, which can be consumed.
	hasSpecificRequirement	:: Bool,				-- ^ Whether at least one specific input datum is required.
	canConsumeAnything	:: Bool					-- ^ Whether at least one arbitrary input datum can be consumed.
} deriving (Eq, Read, Show)

instance ToolShed.SelfValidate.SelfValidator ConsumptionProfile	where
	getErrors c@ MkConsumptionProfile {
		consumptionBounds	= (fewest, most),
		hasSpecificRequirement	= hasSpecificRequirement',
		canConsumeAnything	= canConsumeAnything'
	} = ToolShed.SelfValidate.extractErrors [
		(fewest < 0, "Negative fewest=" ++ show fewest ++ "."),
		(
			case most of
				Just m	-> m < fewest
				_	-> False,
			"Invalid 'most'; " ++ show c ++ "."
		),
		(not $ or [c == zero, hasSpecificRequirement', canConsumeAnything'], "Invalid " ++ show c ++ ".")
	 ]

{-
-- | True if there's no choice in the quantity of data to consume.
isPrecise :: ConsumptionProfile -> Bool
isPrecise	= ConsumptionBounds.isPrecise . consumptionBounds
-}

-- | Predicate, which is 'True' if the specified data-length, falls within the specified 'ConsumptionBounds.ConsumptionBounds'.
withinConsumptionBounds
	:: ConsumptionBounds.DataLength	-- ^ The actual quantity of data consumed.
	-> ConsumptionProfile		-- ^ The bounds within which data-consumption is required to fall.
	-> Bool
withinConsumptionBounds dataLength	= uncurry (&&) . ((dataLength >=) *** Data.Maybe.maybe True (dataLength <=)) . consumptionBounds

-- | Both minimum & maximum set to zero; which can be used as the initial value when accumulating the sum of a list.
zero :: ConsumptionProfile
zero	= MkConsumptionProfile {
	consumptionBounds	= ConsumptionBounds.zero,
	hasSpecificRequirement	= False,
	canConsumeAnything	= False
}

-- | The net effect of two concatenated 'ConsumptionProfile's.
(|+|) :: ConsumptionProfile -> ConsumptionProfile -> ConsumptionProfile
MkConsumptionProfile {
	consumptionBounds	= (lf, ls),
	hasSpecificRequirement	= lh,
	canConsumeAnything	= lc
} |+| MkConsumptionProfile {
	consumptionBounds	= (rf, rs),
	hasSpecificRequirement	= rh,
	canConsumeAnything	= rc
} = MkConsumptionProfile {
	consumptionBounds	= (lf + rf, (+) <$> ls <*> rs),	-- The sum of those of the concatenation.
	hasSpecificRequirement	= lh || rh,			-- The concatenation mandates consumption of at least one specific input datum, if either 'ConsumptionProfile' does.
	canConsumeAnything	= lc || rc			-- The concatenation can consume at least one arbitrary input datum, if either 'ConsumptionProfile' can.
}

-- | The net effect of two alternative 'ConsumptionProfile's.
(<>) :: ConsumptionProfile -> ConsumptionProfile -> ConsumptionProfile
MkConsumptionProfile {
	consumptionBounds	= (lf, ls),
	hasSpecificRequirement	= lh,
	canConsumeAnything	= lc
} <> MkConsumptionProfile {
	consumptionBounds	= (rf, rs),
	hasSpecificRequirement	= rh,
	canConsumeAnything	= rc
} = MkConsumptionProfile {
	consumptionBounds	= (lf `min` rf, max <$> ls <*> rs),	-- Stretched to envelope alternatives.
	hasSpecificRequirement	= lh && rh,				-- The alternation mandates consumption of at least one specific input datum, if both 'ConsumptionProfile's do.
	canConsumeAnything	= lc || rc				-- The alternation can consume at least one arbitrary input datum, if either 'ConsumptionProfile' can.
}

-- | The aggregate of the specified concatenation of 'ConsumptionProfile's.
aggregateFromConcatenation :: [ConsumptionProfile] -> ConsumptionProfile
aggregateFromConcatenation	= Data.List.foldl' (|+|) zero {-initial value-}

-- | The 'ConsumptionProfile's of all the aggregated tails of a list of 'Consumer's.
type AccumulatedConsumptionProfiles	= [ConsumptionProfile]

-- | Accumulate the specified list of 'ConsumptionProfile's, from the specified initial value.
accumulateFrom :: ConsumptionProfile -> [ConsumptionProfile] -> AccumulatedConsumptionProfiles
accumulateFrom	= scanr (|+|)

