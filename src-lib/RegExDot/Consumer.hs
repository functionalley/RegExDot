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

	* Define the requirements & capabilities of a data-type which is designed to consume data.

	* This module also facilitates investigation of group-behaviour, by defining operations on either concatenations or alternations of 'Consumer's.

	* This module is specifically for use in a /regex/-engine,
	in which the meta-data from which the /regex/ is composed, are considered to /consume/ input data,
	whilst concatenation & alternation of such elements, behave as a different types of groups of consumer.
-}

module RegExDot.Consumer(
-- * Type-classes
	Consumer(..),
-- * Types
-- ** Type-synonyms
--	StarHeight,
-- * Functions
	accumulateConsumptionProfiles,
--	accumulateConsumptionProfilesFrom,
	aggregateConsumptionProfilesFromAlternatives,
	aggregateConsumptionProfilesFromConcatenation,
-- ** Query
	getConsumptionBounds,
	getFewest,
	getHasSpecificRequirement
) where

import			RegExDot.ConsumptionProfile((<>), (|+|))
import qualified	Data.List
import qualified	RegExDot.ConsumptionBounds	as ConsumptionBounds
import qualified	RegExDot.ConsumptionProfile	as ConsumptionProfile

{- |
	* A measure of the complexity of a /regex/, which has some baring on either the time-complexity or the space-complexity of the solution.

	* <http://en.wikipedia.org/wiki/Star_height>
-}
type StarHeight	= Int

{- |
	* The interface, to which types, which have a data-requirement, may conform.

	* Regrettably, methods requiring reference to the type of the consumable, would require multi-parameter type-classes.

	* 'consumptionProfile' defines the quantity-range of data which may be consumed, & any specific data-requirements.

	* 'starHeight' measures the complexity of the 'Consumer'.
-}
class Consumer c	where
	consumptionProfile	:: c -> ConsumptionProfile.ConsumptionProfile
	starHeight		:: c -> StarHeight

instance Consumer c => Consumer [c] where
	consumptionProfile	= undefined
	starHeight		= Data.List.foldl' (\l -> max l . starHeight) 0

-- | Convenience-function, to query the 'ConsumptionBounds.ConsumptionBounds' of a 'Consumer'.
getConsumptionBounds :: Consumer c => c -> ConsumptionBounds.ConsumptionBounds
getConsumptionBounds	= ConsumptionProfile.consumptionBounds . consumptionProfile

-- | Determine the minimum acceptable quantity of data.
getFewest :: Consumer c => c -> ConsumptionBounds.DataLength
getFewest	= fst . getConsumptionBounds

-- | Determine whether the specified 'Consumer', has a specific requirement.
getHasSpecificRequirement :: Consumer c => c -> Bool
getHasSpecificRequirement	= ConsumptionProfile.hasSpecificRequirement . consumptionProfile

-- | The aggregate of the specified alternation of 'ConsumptionProfile.ConsumptionProfile's.
aggregateConsumptionProfilesFromAlternatives :: Consumer c => [c] -> ConsumptionProfile.ConsumptionProfile
aggregateConsumptionProfilesFromAlternatives []	= error "RegExDot.Consumer.aggregateConsumptionProfilesFromAlternatives:\tnull list"
aggregateConsumptionProfilesFromAlternatives l	= Data.List.foldl1' (<>) $ consumptionProfile `map` l

-- | Get the 'ConsumptionProfile.ConsumptionProfile's for the specified list of 'Consumer's, then find the net effect of concatenating them.
aggregateConsumptionProfilesFromConcatenation :: Consumer c => [c] -> ConsumptionProfile.ConsumptionProfile
aggregateConsumptionProfilesFromConcatenation	= Data.List.foldl' (\acc -> (acc |+|) . consumptionProfile) ConsumptionProfile.zero {-initial value-}

-- | Get the 'ConsumptionProfile.ConsumptionProfile' for the specified list of 'Consumer's, then accumulate them, from the specified initial value.
accumulateConsumptionProfilesFrom :: Consumer c => ConsumptionProfile.ConsumptionProfile -> [c] -> ConsumptionProfile.AccumulatedConsumptionProfiles
accumulateConsumptionProfilesFrom	= scanr ((|+|) . consumptionProfile)

-- | Get the 'ConsumptionProfile.ConsumptionProfile' for the specified list of 'Consumer's, then accumulate them.
accumulateConsumptionProfiles :: Consumer c => [c] -> ConsumptionProfile.AccumulatedConsumptionProfiles
accumulateConsumptionProfiles	= {-init .-} accumulateConsumptionProfilesFrom ConsumptionProfile.zero	-- It's useful to leave the initial value at the end of the list.

