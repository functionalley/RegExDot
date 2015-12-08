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

	* Defines those options relating to the "RegExDot.RegEx"-runtime.

	* These tend to be options which don't affect the result, only the means by which it is achieved.

	* Whilst similar structures are present in other regex-implementations, there's no standardisation of the fields.
-}

module RegExDot.ExecutionOptions(
-- * Types
-- ** Data-types
	ExecutionOptions(..),
-- * Functions
-- ** Mutators
	setVerbose
) where

import qualified	Data.Default
import qualified	ToolShed.Options

-- | The switches used to control execution of the /regex/-engine.
data ExecutionOptions	= MkExecutionOptions {
	abortTrialRepetitionsOnInherentFailure	:: Bool,	-- ^ If an /alternative/ can't match, irrespective of the subsequent /RegExDot.RegEx.Concatenation/, then avoid futile trial /RegExDot.Repeatable.Repetitions/. The converse of 'checkForUnconsumableData'.
	abortTrialRepetitionsOnZeroConsumption	:: Bool,	-- ^ Check for zero data-consumption by the @n@-th /RegExDot.Repeatable.Repeatable RegExDot.RegEx.CaptureGroup/, before attempting @n+1@.
	bypassInputDataForLiberalConsumer	:: Bool,	-- ^ Whether to bypass reading of the input data, if the mapping to specific /RegExDot.RegEx.RepeatablePattern/s isn't required, & the /RegExDot.RegEx.ExtendedRegEx/ can consume the required quantity of anything.
	catchIncompatibleAnchors		:: Bool,	-- ^ Avoid futile trial solutions, involving repetitions of anchored alternatives, which must consume data.
	checkExistenceOfInelasticTail		:: Bool,	-- ^ If the /RegExDot.RegEx.ExtendedRegEx/ ends in an inelastic (zero /Star-height/) tail, confirm its existence at the end of the /RegExDot.RegEx.InputData/.
	checkForUnconsumableData		:: Bool,	-- ^ Check whether there's no possibility of consuming some of the input data. The converse of 'abortTrialRepetitionsOnInherentFailure'.
	moderateGreed				:: Bool,	-- ^ Greedily consume data, only up to the limit beyond which, future requirements would be compromised.
	permitReorderingOfAlternatives		:: Bool,	-- ^ Permit /RegExDot.RegEx.Alternatives/ to be re-ordered, in an attempt to more quickly locate a result.
	preferAlternativesWhichFeedTheGreedy	:: Bool,	-- ^ Within the /RegExDot.RegEx.MatchedData/ from which each candidate /RegExDot.RegEx.Match/ amongst sequences of /RegExDot.RegEx.Alternatives/, is ultimately composed, prefer /RegExDot.ConsumptionBounds.ConsumptionBounds/ of /RegExDot.RegEx.InputData/, beyond /RegExDot.Repeatable.getFewest/, by /RegExDot.Repeatable.isGreedy RegExDot.RegEx.RepeatablePattern/s.
	preferAlternativesWhichMimickUnrolling	:: Bool,	-- ^ Compare /RegExDot.ConsumptionBounds.ConsumptionBounds/ on successive /RegExDot.Repeatable.Repetitions/ of /RegExDot.RegEx.CaptureGroup/, between candidate /RegExDot.RegEx.Match/es, to mimic the behaviour of the unrolled /RegExDot.Repeatable.Repetitions/.
	preferFewerRepeatedAlternatives		:: Bool,	-- ^ Prefer fewer /RegExDot.Repeatable.Repetitions/ of /RegExDot.RegEx.Alternatives/, to discourage the capture of null lists of /RegExDot.RegEx.InputData/.
	requireMatchList			:: Bool,	-- ^ If merely interested in a 'Bool' result, rather than the optimal mapping of input data to /RegExDot.RegEx.RepeatablePattern/s, avoid unnecessary evaluation of the /RegEx.Match/.
	unrollRepeatedSingletonAlternative	:: Bool,	-- ^ Check whether /RegExDot.RegEx.Alternatives/ consists of just a singleton /RegExDot.RegEx.ExtendedRegEx/, & has therefore been used merely as a capture-group. Though this doesn't affect the result, it vastly improves efficiency.
	useFirstMatchAmongAlternatives		:: Bool,	-- ^ Rather than performing an exhaustive search for the optimal choice amongst /RegExDot.RegEx.Alternatives/, merely select the first that matches; conform to /Perl/ rather than /POSIX/.
	validateMinConsumptionOfAlternatives	:: Bool		-- ^ When the number of repetitions of a /RegExDot.RegEx.CaptureGroup/ is precisely specified, check whether the resulting minimum data-requirement is available.
} deriving (Eq, Show)

instance Data.Default.Default ExecutionOptions	where
	def = setVerbose False $ ToolShed.Options.blankValue {
		abortTrialRepetitionsOnInherentFailure	= True,		-- Regrettably, this slightly reduces performance for most non-pathological patterns.
		catchIncompatibleAnchors		= True,
		checkExistenceOfInelasticTail		= True,
		checkForUnconsumableData		= True,		-- Expensive, particularly when (not requireMatchList), & only typically useful in failure-scenarios.
		moderateGreed				= True,		-- Cost may exceed benefit. TODO: confirm.
		preferAlternativesWhichFeedTheGreedy	= True,
		preferAlternativesWhichMimickUnrolling	= True,
		preferFewerRepeatedAlternatives		= True,
		unrollRepeatedSingletonAlternative	= True,		-- Affects only efficiency, not the result.
		useFirstMatchAmongAlternatives		= False,	-- Perl-style matching may be faster, but may also yield a sub-optimal Match.
		validateMinConsumptionOfAlternatives	= False		-- The cost outweighs the small infrequent dividend.
	}

instance ToolShed.Options.Options ExecutionOptions	where
	blankValue	= MkExecutionOptions {
		abortTrialRepetitionsOnInherentFailure	= undefined,
		abortTrialRepetitionsOnZeroConsumption	= undefined,
		bypassInputDataForLiberalConsumer	= undefined,
		catchIncompatibleAnchors		= undefined,
		checkExistenceOfInelasticTail		= undefined,
		checkForUnconsumableData		= undefined,
		moderateGreed				= undefined,
		permitReorderingOfAlternatives		= undefined,
		preferAlternativesWhichFeedTheGreedy	= undefined,
		preferAlternativesWhichMimickUnrolling	= undefined,
		preferFewerRepeatedAlternatives		= undefined,
		requireMatchList			= undefined,
		unrollRepeatedSingletonAlternative	= undefined,
		useFirstMatchAmongAlternatives		= undefined,
		validateMinConsumptionOfAlternatives	= undefined
	}

-- | Sets those fields which depend crucially on whether the caller wants to retrieve any /RegExDot.RegEx.MatchList/ from the /RegExDot.RegEx.Result/, or just query whether there is one.
setVerbose :: Bool -> ExecutionOptions -> ExecutionOptions
setVerbose verbose e	= e {
	abortTrialRepetitionsOnZeroConsumption	= verbose,	-- The corresponding check, involves evaluation of a /RegExDot.RegEx.MatchList/, which is too expensive if the /RegExDot.RegEx.Matchlist/ isn't otherwise required.
	bypassInputDataForLiberalConsumer	= not verbose,	-- Potentially bypasses reading of /RegExDot.RegEx.InputData/, which is inappropriate if the mapping into a /RegExDot.RegEx.Result/ is required.
	permitReorderingOfAlternatives		= not verbose,	-- Doesn't help when 'requireMatchList', since an exhaustive search of /RegExDot.RegEx.Alternatives/, for the optimal solution, is performed.
	requireMatchList			= verbose
}

