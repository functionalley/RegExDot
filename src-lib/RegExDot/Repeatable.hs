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

	* A data-type, which represents the permissible range of instances, of the underlying polymorphic datum.

	* Designed for use in a polymorphic /regex/-engine, which specifies patterns composed of repeated /greedy/ & /non-greedy/ sequences of /Meta/-data;

 >		*	+	?	{fewest, most}	{fewest,}	{fewest}
 >		*?	+?	??	{fewest, most}?	{fewest,}?

	* In the context of /regex/es, this concept is known as /Quantification/.

	* /regex/es evolved from the minimal ability to optionally qualify the datum with a <http://en.wikipedia.org/wiki/Kleene_star> suffix.
	More exotic repetition-specifications could be composed by concatenating these atomic building-blocks.
	Here, I've taken the contrary top-down view, & assumed that all data are qualified by a full 'RepetitionBounds', which in most cases will degenerate into a simpler form.

	* The type of entity which is being repeated, isn't the domain of this data-type; it's polymorphic.
-}

module RegExDot.Repeatable(
-- * Types
-- ** Type-synonyms
	Repetitions,
	RepetitionBounds,
-- ** Data-types
	Repeatable(..),
-- * Constants
--	nonGreedyToken,
	oneOrMoreToken,
	rangeDelimiters,
	rangeSeparatorToken,
	tokens,
	zeroOrMoreToken,
	zeroOrOneToken,
-- * Functions
	one,
	oneOrMore,
	oneOrMore',
	zeroOrMore,
	zeroOrMore',
	zeroOrOne,
	zeroOrOne',
--	precisely,
	repeatableParser,
	showSuffix,
-- ** Accessors
	getFewest,
	getMost,
-- ** Mutators
	focus,
--	setNonGreedy,
	toSingleton,
-- ** Operators
	(^#->#),
	(^#->#?),
	(^#->),
	(^#->?),
	(^#),
-- ** Predicates
	isPrecise,
	hasPreciseBounds
) where

import			Control.Arrow((***))
import qualified	Control.DeepSeq
import qualified	Data.List
import qualified	RegExDot.Consumer		as Consumer
import qualified	RegExDot.ConsumptionProfile	as ConsumptionProfile
import qualified	Text.ParserCombinators.Parsec	as Parsec
import			Text.ParserCombinators.Parsec((<?>))
import qualified	ToolShed.Data.Pair
import qualified	ToolShed.SelfValidate

#if !MIN_VERSION_base(4,8,0)
import	Control.Applicative((<$>), (<*>))
#endif

infix 6 ^#->#, ^#->#?, ^#->, ^#->?, ^#	-- A notch tighter than "DSL"s binary operators.

-- | A number of repetitions.
type Repetitions	= Int

-- | Defines the bounds of a range of permissible repetitions.
type RepetitionBounds	= (Repetitions, Maybe Repetitions)

-- | Creates a precise 'RepetitionBounds', i.e. both lower & upper bounds on the number of 'Repetitions' are equal to the same value.
precisely :: Repetitions -> RepetitionBounds
precisely i	= (i, Just i)

-- | Predicate which is 'True' if exactly one value is permissible, ie lower & upper bounds on the number of 'Repetitions' are identical.
hasPreciseBounds :: RepetitionBounds -> Bool
hasPreciseBounds (fewest, most)	= Just fewest == most

-- | Declares a polymorphic data-type, which augments the underlying 'base' datum, with the range of times it may be used.
data Repeatable a	= MkRepeatable {
	base			:: a,			-- ^ The underlying polymorphic datum.
	repetitionBounds	:: RepetitionBounds,	-- ^ The bounds delimiting the range of permissible repetitions, of 'base'.
	isGreedy		:: Bool			-- ^ Whether to demand as many matching instances of 'base' as possible; or as few (AKA /lazy quantification/).
} deriving Eq

instance Functor Repeatable	where
	fmap f repeatable	= repeatable { base = f $ base repeatable }

-- | True if there's no choice in the number of repetitions; implemented via 'isPrecise'.
isPrecise :: Repeatable a -> Bool
isPrecise	= hasPreciseBounds . repetitionBounds

-- | Builds a parser for a specification of the number of permissible instances of the specified polymorphic parameter.
repeatableParser :: a -> Parsec.Parser (Repeatable a)
repeatableParser b	= Parsec.option (
	one b	-- The default; there's no concept of greediness here.
 ) $ do
	repeatable	<- Parsec.choice [
		(Parsec.char oneOrMoreToken <?> "Repeatable.oneOrMoreToken " ++ show oneOrMoreToken)	>> return {-to ParsecT-monad-} (oneOrMore b),
		(Parsec.char zeroOrOneToken <?> "Repeatable.zeroOrOneToken " ++ show zeroOrOneToken)	>> return {-to ParsecT-monad-} (zeroOrOne b),
		(Parsec.char zeroOrMoreToken <?> "Repeatable.zeroOrMoreToken " ++ show zeroOrMoreToken)	>> return {-to ParsecT-monad-} (zeroOrMore b),
		(b ^#->#) {-arbitrarily greedy for now-} <$> uncurry Parsec.between (ToolShed.Data.Pair.mirror Parsec.char rangeDelimiters) (
			do
				fewest	<- Parsec.spaces >> (read <$> Parsec.many1 Parsec.digit <?> "Repetition-range minimum")
				most	<- Parsec.spaces >> Parsec.option (
					Just fewest	-- The default.
				 ) (
					do
						i	<- (
							Parsec.char rangeSeparatorToken			<?> "Repeatable.rangeSeparatorToken " ++ show rangeSeparatorToken
						 ) >> Parsec.spaces >> Parsec.option Nothing {-default to open-ended range-} (
							Just . read <$> Parsec.many1 Parsec.digit	<?> "Repetition-range maximum"
						 )

						Parsec.spaces >> return {-to ParsecT-monad-} i
				 )

				return {-to ParsecT-monad-} (fewest, most)
		) <?> "Repeatable.rangeDelimiters " ++ show rangeDelimiters
	 ]

	g	<- Parsec.option True {-the default-} $ (Parsec.char nonGreedyToken <?> "Repeatable.nonGreedyToken " ++ show nonGreedyToken) >> return {-to ParsecT-monad-} False

	return {-to ParsecT-monad-} repeatable { isGreedy = g }	-- Correct prior assumption.

instance Read a => Read (Repeatable a)	where
	readsPrec _ s	= case reads s {-first, read the base-type-} of
		[(base', s1)]	-> (error . ("readsPrec Repeatable:\tparse-error; " ++) . show) `either` return $ Parsec.parse ((,) <$> repeatableParser base' <*> Parsec.getInput) "Repeatable" s1
		_		-> []	-- No parse.

{- |
	* A 'ShowS'-function for the suffix, denoting the permissible repetitions, of 'base'.

	* This function converts the internal, into the tradition /greedy/ & /non-greedy/ quantifiers of various specific varieties.
-}
showSuffix :: Repeatable a -> ShowS
showSuffix repeatable	= let
	showRange :: ShowS -> ShowS
	showRange x	= (\(begin, end) -> begin . x . end) $ ToolShed.Data.Pair.mirror showChar rangeDelimiters
 in (
	case repetitionBounds repeatable of
		(0, Nothing)		-> showChar zeroOrMoreToken
		(1, Nothing)		-> showChar oneOrMoreToken
		(fewest, Nothing)	-> showRange $ shows fewest . showChar rangeSeparatorToken
		(0, Just 1)		-> showChar zeroOrOneToken
		(1, Just 1)		-> id	-- CAVEAT: since there's no explicit repetition-operator, the non-greedy modifier can't be appended.
		(fewest, Just most)	-> showRange $ if fewest == most
			then shows fewest	-- Single-valued range.
			else shows fewest . showChar rangeSeparatorToken . shows most
 ) . if ($ repeatable) `any` [isGreedy, isPrecise] {-without a range of possibilities, non-greediness is irrelevant-}
	then id
	else showChar nonGreedyToken	-- This can only be appended, if there a previous repetition-operator for it to modify.

-- Replicate the syntax, for repetition, as used in a POSIX-standard /regex/.
instance Show a => Show (Repeatable a)	where
	showsPrec _ repeatable	= shows (base repeatable) . showSuffix repeatable

instance Consumer.Consumer a => Consumer.Consumer (Repeatable a)	where
	consumptionProfile MkRepeatable {
		base			= b,
		repetitionBounds	= (fewest, most)
	} = baseConsumptionProfile {
		ConsumptionProfile.consumptionBounds	= (fewest *) *** ((*) <$> most <*>) $ ConsumptionProfile.consumptionBounds baseConsumptionProfile	-- CAVEAT: special cases exist, where one or both halves of this calculation degenerate to a simpler form, but special treatment, in an attempt to improve performance, proved counterproductive.
	} where
		baseConsumptionProfile :: ConsumptionProfile.ConsumptionProfile
		baseConsumptionProfile	= Consumer.consumptionProfile b

	starHeight MkRepeatable {
		base			= b,
		repetitionBounds	= r
	} = Consumer.starHeight b + if hasPreciseBounds r then 0 else 1

instance ToolShed.SelfValidate.SelfValidator a => ToolShed.SelfValidate.SelfValidator (Repeatable a)	where
	getErrors MkRepeatable {
		base			= b,
		repetitionBounds	= (fewest, most),
		isGreedy		= g
	}
		| not $ ToolShed.SelfValidate.isValid b	= ToolShed.SelfValidate.getErrors b	-- Delegate.
		| otherwise				= ToolShed.SelfValidate.extractErrors [
			(fewest < 0, "Negative fewest=" ++ show fewest ++ "."),
			(
				case most of
					Just m	-> m < fewest
					_	-> False,
				"Invalid repetition-range; '" ++ show (fewest, most) ++ "'."
			), (
				not g && case most of
					Just m	-> fewest >= m	-- There ought to be potential for non-greediness, where specified: the converse isn't true, since greediness isn't explicit, & may not have been wanted.
					_	-> False,
				"Invalid non-greedy repetition-range; '" ++ show (fewest, most) ++ "'."
			)
		]

instance Control.DeepSeq.NFData a => Control.DeepSeq.NFData (Repeatable a)	where
	rnf MkRepeatable {
		base			= b,
		repetitionBounds	= r,
		isGreedy		= g
	} = Control.DeepSeq.rnf (b, r, g)

-- | Mutator.
setNonGreedy :: Repeatable a -> Repeatable a
setNonGreedy r	= r { isGreedy = False }

{- |
	* Construct a greedy 'Repeatable', from a polymorphic datum, with the specified range of permissible instances.

	* The /#/s in the identifier represent the two bounds.

	* /a{f, m}/
-}
(^#->#)
	:: a			-- ^ The polymorphic payload from which to construct the 'Repeatable'.
	-> RepetitionBounds	-- ^ The permissible repetition-bounds for the polymorphic data.
	-> Repeatable a
b ^#-># bounds	= MkRepeatable {
	base			= b,
	repetitionBounds	= bounds,
	isGreedy		= True
}

{- |
	* Construct a non-greedy version of '^#->#'.

	* /a{f, m}?/
-}
(^#->#?)
	:: a			-- ^ The polymorphic payload from which to construct the 'Repeatable'.
	-> RepetitionBounds	-- ^ The permissible repetition-bounds for the polymorphic data.
	-> Repeatable a
b ^#->#? bounds	= setNonGreedy (b ^#-># bounds)

{- |
	* Construct a greedy 'Repeatable', tailored for data repeated at least the specified number of times.

	* The /#/ in the identifier represents the single bound.

	* /a{f,}/
-}
(^#->)
	:: a		-- ^ The polymorphic payload from which to construct the 'Repeatable'.
	-> Repetitions	-- ^ The minimum permissible repetitions of the polymorphic data.
	-> Repeatable a
b ^#-> fewest	= b ^#-># (fewest, Nothing)

{- |
	* Construct a non-greedy version of '^#->'.

	* /a{f,}?/
-}
(^#->?)
	:: a		-- ^ The polymorphic payload from which to construct the 'Repeatable'.
	-> Repetitions	-- ^ The minimum permissible repetitions of the polymorphic data.
	-> Repeatable a
b ^#->? fewest	= setNonGreedy (b ^#-> fewest)

{- |
	* Construct a 'Repeatable', tailored for data repeated a precise number of times.

	* The /#/ in the identifier represents the single bound.

	* /a{f}/
-}
(^#)
	:: a		-- ^ The polymorphic payload from which to construct the 'Repeatable'.
	-> Repetitions	-- ^ The precise number of repetitions of the polymorphic data which is required.
	-> Repeatable a
b ^# r	= b ^#-># precisely r

{- |
	* Construct a 'Repeatable', tailored for unrepeated data.

	* A degenerate case of '^#'.
-}
one :: a -> Repeatable a
one	= (^# 1)

{- |
	* Construct a greedy 'Repeatable', from a polymorphic datum, with 'fewest' == 0 & 'most' == 1.

	* A specific case of '^#->#'.
-}
zeroOrOne :: a -> Repeatable a
zeroOrOne	= (^#-># (0, Just 1))

-- | Construct a non-greedy version of 'zeroOrOne'.
zeroOrOne' :: a -> Repeatable a
zeroOrOne'	= setNonGreedy . zeroOrOne

{- |
	* Construct a greedy 'Repeatable', from a polymorphic datum, with 'fewest' == 0.

	* A specific case of '^#->'.
-}
zeroOrMore :: a -> Repeatable a
zeroOrMore	= (^#-> 0)

-- | Construct a non-greedy version of 'zeroOrMore'.
zeroOrMore' :: a -> Repeatable a
zeroOrMore'	= setNonGreedy . zeroOrMore

{- |
	* Construct a greedy 'Repeatable', from a polymorphic datum, with lower 'RepetitionBounds' == one.

	* A specific case of '^#->'.
-}
oneOrMore :: a -> Repeatable a
oneOrMore	= (^#-> 1)

-- | Construct a non-greedy version of 'oneOrMore'.
oneOrMore' :: a -> Repeatable a
oneOrMore'	= setNonGreedy . oneOrMore

-- | Reduces a 'Repeatable', with a range of 'RepetitionBounds', to a precise number of repetitions.
focus :: Repeatable a -> Repetitions -> Repeatable a
focus r i	= r { repetitionBounds = precisely i }

{- |
	* Reduces a 'Repeatable', with a range of 'RepetitionBounds', to a singleton.

	* A degenerate case of 'focus'.
-}
toSingleton :: Repeatable a -> Repeatable a
toSingleton	= (`focus` 1)

-- | Accessor.
getFewest :: Repeatable a -> Repetitions
getFewest MkRepeatable { repetitionBounds = (f, _) }	= f

-- | Accessor.
getMost :: Repeatable a -> Maybe Repetitions
getMost MkRepeatable { repetitionBounds = (_, m) }	= m

-- | The token used to denote /non-greedy/, when in the 'String'-form.
nonGreedyToken :: Char
nonGreedyToken	= '?'

{- |
	* The token used to denote 'zeroOrMore', when in the 'String'-form.

	* AKA /Kleene Star/.
-}
zeroOrMoreToken :: Char
zeroOrMoreToken	= '*'

-- | The token used to denote 'zeroOrOne', when in the 'String'-form.
zeroOrOneToken :: Char
zeroOrOneToken	= '?'

-- | The token used to denote 'oneOrMore', when in the 'String'-form.
oneOrMoreToken :: Char
oneOrMoreToken	= '+'

-- | The delimiters of '^#->#', when in the 'String'-form.
rangeDelimiters :: (Char, Char)
rangeDelimiters	= ('{', '}')

-- | The token used to separate 'RepetitionBounds', when in the 'String'-form.
rangeSeparatorToken :: Char
rangeSeparatorToken	= ','

-- | The set of 'Char' to which a specific meaning is attributed, when reading from 'String'.
tokens :: String
tokens	= Data.List.nub [nonGreedyToken, zeroOrMoreToken, zeroOrOneToken, oneOrMoreToken, fst rangeDelimiters, snd rangeDelimiters, rangeSeparatorToken]

