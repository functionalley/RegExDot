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

	* This implementation of extended /regex/es, generalises the familiar concept of pattern-matching of character-strings, to matching lists composed from an arbitrary data-type.
	The polymorphic data, from which the input data-list is composed, need only support @Eq@.

	* Because of the unknown stringified form of the underlying polymorphic data,
	the /regex/ must be described by a comparatively verbose bracketed & comma-separated list, rather than the traditional /String/ containing Meta-characters.
	Each element of this 'Concatenation' is a 'RepeatablePattern', which describes a permissible match against 'InputData'.

	* 'RepeatablePattern' can take one of two forms.
	In the simplest case, it matches just a single item of the underlying polymorphic type, perhaps literally, though looser specifications also exist:
	@.@ matches any input datum; @[x, y, z]@ matches any of @x@, @y@, or @z@; @[^x, y, z]@ matches anything but @x@, @y@, or @z@.
	To support POSIX /ERE/s, 'RepeatablePattern' can also be a list 'Alternatives', each of which is recursively defined as an 'ExtendedRegEx', to form a tree-structure.

	* Each 'Pattern', can optionally be /quantified/ by either a traditional /greedy/, or a /Perl/-style /non-greedy/, suffix, e.g.; @[*, +, ?, {n, m}, {n,}, {n}, *?, +?, ??, {n, m}?, {n,}?]@.

	* For convenience, common specifications can be canned & assigned a single 'Char' mnemonic, for subsequent reference.
	Since 'ExtendedRegEx' is polymorphic, the set of abbreviations appropriate in the context of the unspecified base-type,
	must be implemented externally through the 'Meta.ShortcutExpander' interface.
	This permits the use, when the type-parameter is 'Char', of /Perl-style shortcuts/ @[\\d\\D\\s\\S\\w\\W]@.

	* The algorithm, is the classic /back-tracking/ one, rather than either a /DFA/ or /NFA/.
	This permits construction of 'Result' via which one can discover the deep mapping of 'InputData' into 'ExtendedRegEx',
	& provides the flexibility to add the features now expected by modern /regex/-engines.
	Since the type-parameter is unknown, & may represent a large object, the exponential space-complexity of creating a /DFA/ may present additional problems.
	The exponential time-complexity of the /back-tracking/ algorithm is partially tamed by targeting obvious inefficiencies with specific optimisations.

	* Char-based regexen, traditionally overload the delimiters of a set of 'Alternatives' (parentheses), as a request for data-capture.
	Here, in contrast, all 'RepeatablePattern's capture data, & repeated sub-expressions capture a list of data,
	rather than arbitrarily recording just the last (<http://www.opengroup.org/onlinepubs/009695399/functions/regcomp.html>) item.

 [@REFERENCES@]

	* <https://en.wikipedia.org/wiki/Regular_expression>

	* <http://swtch.com/~rsc/regexp/regexp1.html>

	* <http://docstore.mik.ua/orelly/perl/prog/ch02_04.htm#PERL2-CH-2-SECT-4.1.2>

	* <http://www.macs.hw.ac.uk/~dsg/gph/papers/html/Strategies/strategies.html>

	* <http://hackage.haskell.org/packages/archive/regex-posix/latest/doc/html/Text-Regex-Posix.html>

	* <http://www.haskell.org/haskellwiki/Regular_expressions>

	* <http://www2.research.att.com/~gsf/testregex/re-interpretation.html>

 [@CAVEATS@]

	* Because of the definition of mutually recursive data-types, it is difficult to split this annoyingly large module, & preserve compatibility across compilers,
	but it may be possible to break this cyclic dependency, by defining an interface to which one of the data-types defined here conforms.

	* Doesn't implement /Back-references/, making the definition of the 'ExtendedRegEx' context-free.

	* There's no integration with the type-classes defined in "Text.Regex.Base.RegexLike", which assumes 'Char'-based 'InputData';
	though this could be added to a specialised instance.

	* When 'Alternatives' are defined, 'Result' becomes a tree-like structure.
	Unless the alternative is a singleton, the specific alternative selected in the solution is typically unknown, & therefore the /structure/ of the branch of this tree is also unknown.
	This lack of clarity is compounded when the 'Alternatives' are 'Repeatable.Repeatable', since a different one may be selected on each successive repetition.
	Consequently, the user can't navigate this portion of the structure in a statically defined manner, to acquire the captured data.
	Despite this, & in contrast to other /regex/-engines, access to the whole data-structure is available, since it doesn't seem advantage to hide it.
	The user can then either use 'extractDataFromMatch' for that element of 'Result', thus aggregating the data from sections of unknown structure, or 'show' it, as an aid to debugging.

 [@TODO@]

	* Test parallel-operation, on a 3 or more processor machine.
	If 'rnf' is less effective than 'rwhnf',
	then the 'Control.DeepSeq.NFData' context can be removed,
	reducing the requirements imposed on the type-parameter 'a'.

	* Try 'Data.List.Stream' (stream-fusion), a faster drop-in replacement for 'Data.List'; possibly integrated in GHC-6.12.

	* 'ExecutionOptions.bypassInputDataForLiberalConsumer' is too restrictive.
	More generally, we can test whether the set of different 'a' in 'InputData', is a subset of those common to all remaining terms in the 'ExtendedRegEx'.
	Using this rule, we can infer @"aaa ..." =~ MkExtendedRegEx [a,a+,a?,[ab]{2,3}]@, given compatible 'ConsumptionProfile.consumptionBounds'.

	* Nested repetitions, where nothing has been added to the expression, result in repeated trials of the same expression,
	e.g.; @"(x{i,}){j,}"@ results in the same expansion for @(i, j) in [(2, 3), (3, 2), (6, 1), (1, 6)]@.
	The resulting 'MatchList' may be different, but if the first such trial fails, so will all the remainder.

	* Should cope with empty sets of 'Alternatives' & zero repetitions, neither of which can ever match, but the wider pattern can, e.g. @(()|x{0}|y)@.

	* By removing 'RepeatablePattern' from 'Match', it can be isolated in a new module.
	This would result in a significant loss of discoverability.

	* Expand repeated 'Anchor.Bow' with @fewest - 1@ null matches followed by recursive 'findMatch'-call with @repetitions = 1@.
-}

module RegExDot.RegEx(
-- * Type-classes
	ShortcutExpander(..),
-- * Types
-- ** Type-synonyms
--	BasicRegEx,
	Concatenation,
	ExternalMatch,
	InputData,
	MatchedData,
	MatchList,
--	MetaDataList,
	RepeatablePattern,
--	Transformation,
-- ** Data-types
	Alternatives(
		MkAlternatives,
		deconstructAlternatives
	),
	Match,
	ExtendedRegEx(..),
	Pattern(..),
	Result,
-- * Constants
	alternativeExtendedRegExSeparatorToken,
--	anyDatum,
	captureGroupDelimiters,
	tokens,
-- * Functions
--	accumulateDistinctInputData,
--	accumulateDistinctMetaDataFrom,
	dock,
--	drift,
--	findMatch,
	captureGroup,
--	mkNullMatchFromConcatenation,
--	mkNullMatchFromExtendedRegEx,
--	mkNullMatchFromRepeatablePattern,
--	safeReciprocal,
--	shiftMatchedData,
--	shiftMatch,
	shiftMatchList,
	showsMaybeAnchor,
	simply,
	transformExtendedRegEx,
-- ** Accessors
--	getInputData,
-- ** Operators
	(+~),
	(=~),
	(/~),
	(.*),
	(.*?),
--	(/+)
-- ** Predicates
--	hasBowAnchor,
--	hasSternAnchor,
	isDefined,
	isCaptureGroup,
	isSingletonAlternatives,
--	isUnconsumableByAnyOf,
-- ** Query
--	getDistinctMetaDataFromAlternatives,
--	getDistinctMetaDataFromConcatenation,
--	getDistinctMetaDataFromExtendedRegEx,
--	getDistinctMetaDataFromPattern,
--	getDistinctMetaDataFromRepeatablePattern,
	externalMatchLength,
	extractDataFromMatch,
	extractDataFromMatch',
	extractDataFromMatchList
) where

import			Control.Arrow((&&&))
import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Control.Parallel.Strategies
import qualified	Data.Char
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Maybe
import qualified	Data.Ord
import qualified	RegExDot.Anchor			as Anchor
import qualified	RegExDot.Consumer		as Consumer
import qualified	RegExDot.ConsumptionBounds	as ConsumptionBounds
import qualified	RegExDot.ConsumptionProfile	as ConsumptionProfile
import qualified	RegExDot.ExecutionOptions	as ExecutionOptions
import qualified	RegExDot.Meta			as Meta
import qualified	RegExDot.RegExOpts		as RegExOpts
import qualified	RegExDot.Repeatable		as Repeatable
import			RegExDot.Repeatable((^#->))
import qualified	RegExDot.Tree			as Tree
import qualified	ToolShed.Data.List
import qualified	ToolShed.Data.List.Splits
import qualified	ToolShed.SelfValidate

infix 4 +~, =~, /~	-- Same as (==) & (/=).

-- | The type of a /regex/, in which there's no provision for either 'Alternatives' or 'Anchor.Anchor's.
type BasicRegEx m	= [Repeatable.Repeatable (Meta.Meta m)]

{- |
	* Defines the method required to expand a mnemonic into an 'ExtendedRegEx'.

	* CAVEAT: this interface must be declared locally, since it references 'ExtendedRegEx', & 'ExtendedRegEx' references it.
-}
class ShortcutExpander m	where
	expand	:: Char -> ExtendedRegEx m	-- ^ Expand a single-'Char' mnemonic into the corresponding 'ExtendedRegEx'.

{- |
	* Represents the /alternation/ feature of 'ExtendedRegEx's.

	* One could amalgamate this with 'Pattern', since it seems to exist merely as a peg to hang /instance/-declarations from.
-}
newtype Alternatives m	= MkAlternatives {
	deconstructAlternatives	:: [ExtendedRegEx m]
} deriving Eq

-- | Similar to 'fmap', but operates on ['ExtendedRegEx'], rather than just @a@.
transformAlternatives
	:: ([ExtendedRegEx m] -> [ExtendedRegEx m])	-- ^ The transformation function to apply to the list of data behind the constructor.
	-> Alternatives m
	-> Alternatives m
transformAlternatives f	= MkAlternatives . f . deconstructAlternatives

instance (
	ShortcutExpander	m,
	Meta.ShortcutExpander	m,
	Eq			m,
	Read			m
 ) => Read (Alternatives m)	where
	readsPrec _ (' ' : s)	= reads s	-- Consume white-space.
	readsPrec _ ('\t' : s)	= reads s	-- Consume white-space.
	readsPrec _ s		= case reads s of
		[(extendedRegEx, s1)]	-> case dropWhile Data.Char.isSpace s1 of
			'|' : s2	-> Control.Arrow.first (transformAlternatives (extendedRegEx :)) `map` reads s2 {-singleton-}
			_		-> [(MkAlternatives [extendedRegEx], s1)]
		_			-> []	-- No parse.

instance Show m => Show (Alternatives m)	where
	showsPrec _	= foldl (.) id . Data.List.intersperse (showChar alternativeExtendedRegExSeparatorToken) . map shows . deconstructAlternatives	-- Replace the default list-format, with 'egrep'-syntax.

instance Consumer.Consumer (Alternatives m)	where
	consumptionProfile	= Consumer.aggregateConsumptionProfilesFromAlternatives . deconstructAlternatives
	starHeight		= Consumer.starHeight . deconstructAlternatives	-- Must evaluate all Alternatives to determine best.

instance ToolShed.SelfValidate.SelfValidator (Alternatives m)	where
	getErrors	= ToolShed.SelfValidate.getErrors . deconstructAlternatives

instance Control.DeepSeq.NFData m => Control.DeepSeq.NFData (Alternatives m)	where
	rnf	= Control.DeepSeq.rnf . deconstructAlternatives

-- | 'Alternatives' can be employed as a simple /capture-group/ as well as a switch, under which circumstances there's no choice amongst multiple 'Alternatives'.
isSingletonAlternatives :: Alternatives m -> Bool
isSingletonAlternatives	= (== 1) . length . deconstructAlternatives

-- | A set of 'Meta.Meta' which can be consumed.
type MetaDataList m	= [Meta.Meta m]

{- |
	* 'True' if there's no possibility, that the specified input datum, can be consumed by any of the specified 'MetaDataList'.

	* Because data may not be consumed, perhaps because of unsuitable 'ConsumptionBounds.ConsumptionBounds', the converse doesn't hold,
	i.e. @not isUnconsumableByAnyOf@ /doesn't/ imply that the input datum can be consumed, just that it hasn't been proven that it can't.
-}
isUnconsumableByAnyOf :: Eq m
	=> m			-- ^ The input datum.
	-> MetaDataList m	-- ^ The list of meta-data against any of which a match can occur.
	-> Bool
isUnconsumableByAnyOf i	= not . any (Meta.isMatch i)

-- | The set of distinct 'Meta.Meta', in the specified 'Alternatives'.
getDistinctMetaDataFromAlternatives :: Eq m => Alternatives m -> MetaDataList m
getDistinctMetaDataFromAlternatives	= foldr (Data.List.union . getDistinctMetaDataFromExtendedRegEx) [] . deconstructAlternatives

-- | Defines either a simple 'Meta.Meta', which can match exactly one datum, or a set of 'Alternatives', each of which is recursively defined above, as an 'ExtendedRegEx'.
data Pattern m	=
	Require (Meta.Meta m)		-- ^ Describes a requirement for a simple scalar datum of the polymorphic type.
	| CaptureGroup (Alternatives m)	-- ^ A sub-expression containing a selection of recursively defined alternatives, thus forming a tree-structure.
	deriving Eq

instance (
	Eq			m,
	Meta.ShortcutExpander	m,
	Read			m,
	ShortcutExpander	m
 ) => Read (Pattern m)	where
	readsPrec _ (' ' : s)	= reads s	-- Consume white-space.
	readsPrec _ ('\t' : s)	= reads s	-- Consume white-space.
	readsPrec _ ('(' : s)	= case {-Alternatives.-} reads s of
		[(alternatives, s1)]	-> case dropWhile Data.Char.isSpace s1 of
			')' : s2	-> [(CaptureGroup alternatives, s2)]
			_		-> []	-- No parse.
		_			-> []	-- No parse.
	readsPrec _ s		= case reads s of
		[pair]	-> [Control.Arrow.first Require pair]
		_	-> []	-- No parse.

instance Show m => Show (Pattern m)	where
	showsPrec _ (Require meta)		= shows meta
	showsPrec _ (CaptureGroup alternatives)	= showChar (fst captureGroupDelimiters) . shows alternatives . showChar (snd captureGroupDelimiters)

instance Consumer.Consumer (Pattern m)	where
	consumptionProfile (Require meta)		= Consumer.consumptionProfile meta
	consumptionProfile (CaptureGroup alternatives)	= Consumer.consumptionProfile alternatives

	starHeight (Require meta)		= Consumer.starHeight meta
	starHeight (CaptureGroup alternatives)	= Consumer.starHeight alternatives

instance ToolShed.SelfValidate.SelfValidator (Pattern m)	where
	getErrors (Require meta)		= ToolShed.SelfValidate.getErrors meta
	getErrors (CaptureGroup alternatives)	= ToolShed.SelfValidate.getErrors alternatives

instance Control.DeepSeq.NFData m => Control.DeepSeq.NFData (Pattern m)	where
	rnf (Require meta)		= Control.DeepSeq.rnf meta
	rnf (CaptureGroup alternatives)	= Control.DeepSeq.rnf alternatives

-- | Convenience-function to build a 'CaptureGroup' from a list of alternative 'ExtendedRegEx's.
captureGroup :: [ExtendedRegEx m] -> Pattern m
captureGroup	= CaptureGroup . MkAlternatives

-- | True if the 'Pattern' was constructed via 'CaptureGroup'.
isCaptureGroup :: Pattern m -> Bool
isCaptureGroup (CaptureGroup _)	= True
isCaptureGroup _		= False

-- | The set of distinct 'Meta.Meta', in the specified 'Pattern'.
getDistinctMetaDataFromPattern :: Eq m => Pattern m -> MetaDataList m
getDistinctMetaDataFromPattern pat	= case pat of
	Require meta			-> [meta]
	CaptureGroup alternatives	-> getDistinctMetaDataFromAlternatives alternatives

-- | Constant pattern, representing a lax 'Require'ment.
anyDatum :: Pattern a
anyDatum	= Require Meta.Any

-- | Make 'Pattern's, 'Repeatable.Repeatable'.
type RepeatablePattern m	= Repeatable.Repeatable (Pattern m)

-- | Construct a null 'Match' by assuming that the specified 'RepeatablePattern' consumes zero 'InputData'; of which the caller should ensure it's capable.
mkNullMatchFromRepeatablePattern
	:: ConsumptionBounds.DataLength	-- ^ The offset into the list of input data at which the zero-length match occurred.
	-> RepeatablePattern m		-- ^ The pattern that the input data matched.
	-> Match m			-- ^ The resulting match-structure.
mkNullMatchFromRepeatablePattern offset r	= case Repeatable.base r of
	Require _	-> Tree.Leaf (r, offset, [])
	CaptureGroup _	-> Tree.Node $ Repeatable.getFewest r `replicate` []

-- | The set of distinct 'Meta.Meta', in the specified 'RepeatablePattern'.
getDistinctMetaDataFromRepeatablePattern :: Eq m => RepeatablePattern m -> MetaDataList m
getDistinctMetaDataFromRepeatablePattern	= getDistinctMetaDataFromPattern . Repeatable.base

{- |
	* Represents a black hole, which will greedily consume all data.

	* CAVEAT: nullary, i.e. a constant.
-}
(.*) :: RepeatablePattern m
(.*)	= Repeatable.zeroOrMore anyDatum

-- | A /non-greedy/ version of '.*'.
(.*?) :: RepeatablePattern m
(.*?)	= Repeatable.zeroOrMore' anyDatum

-- | Convenience-function, to build a 'RepeatablePattern' from an unrepeated instance of the specified 'Meta.Meta'-datum.
simply :: Meta.Meta m -> RepeatablePattern m
simply	= Repeatable.one . Require

-- | Represents the /concatenation/ aspect of 'ExtendedRegEx's.
type Concatenation m	= [RepeatablePattern m]

-- | Construct a null 'Match' by assuming that the specified 'Concatenation' consumes zero 'InputData'.
mkNullMatchFromConcatenation
	:: ConsumptionBounds.DataLength	-- ^ The offset into the list of input data at which the zero-length match occurred.
	-> Concatenation m		-- ^ The list of repeatable patterns that the input data matched.
	-> MatchList m			-- ^ The resulting match-structure.
mkNullMatchFromConcatenation offset	= map $ mkNullMatchFromRepeatablePattern offset

-- | The set of distinct 'Meta.Meta', in the specified 'Concatenation'.
getDistinctMetaDataFromConcatenation :: Eq m => Concatenation m -> MetaDataList m
getDistinctMetaDataFromConcatenation	= foldr (Data.List.union . getDistinctMetaDataFromRepeatablePattern) []

-- | The accumulating set of 'MetaDataList', resulting from each successive 'RepeatablePattern' in the specified 'Concatenation'.
accumulateDistinctMetaDataFrom :: Eq m => MetaDataList m -> Concatenation m -> [MetaDataList m]
accumulateDistinctMetaDataFrom	= scanr (Data.List.union . getDistinctMetaDataFromRepeatablePattern)

-- | Constructs an 'ExtendedRegEx', by surrounding a 'Concatenation' with optional 'Anchor.Anchor's.
data ExtendedRegEx m	= MkExtendedRegEx {
	bowAnchor	:: Maybe Anchor.Anchor,	-- ^ An option to anchor the /regex/ to the start of the 'InputData'.
	concatenation	:: Concatenation m,	-- ^ The sequence of 'RepeatablePattern's defining the 'Require'ments that the 'InputData' must meet.
	sternAnchor	:: Maybe Anchor.Anchor	-- ^ An option to anchor the /regex/ to the end of the 'InputData'.
} deriving Eq

-- | The type of a function which transforms an 'ExtendedRegEx'.
type Transformation m	= ExtendedRegEx m -> ExtendedRegEx m

-- | Similar to 'fmap', but operates on 'Concatenation', rather than just @a@.
transformExtendedRegEx
	:: (Concatenation m -> Concatenation m)	-- ^ The function used to transform the data behind the constructor.
	-> Transformation m
transformExtendedRegEx f extendedRegEx	= extendedRegEx { concatenation	= f $ concatenation extendedRegEx }

instance (
	Eq			m,
	Meta.ShortcutExpander	m,
	Read			m,
	ShortcutExpander	m
 ) => Read (ExtendedRegEx m)	where
	readsPrec _ []			= []
	readsPrec _ (' ' : s)		= reads s	-- Consume white-space.
	readsPrec _ ('\t' : s)		= reads s	-- Consume white-space.
	readsPrec _ ('\\' : c : s)	= [(expand c, s)]
	readsPrec _ s			= case singleton of
		[(extendedRegEx, _)]
			| ToolShed.SelfValidate.isValid extendedRegEx	-> singleton
			| otherwise					-> error $ ToolShed.SelfValidate.getFirstError extendedRegEx	-- Parsed OK, but invalid.
		_							-> []								-- No parse.
		where
--			singleton :: (Eq m, Meta.ShortcutExpander m, Read m, ShortcutExpander m) => [(ExtendedRegEx m, String)]
			singleton = [
				(
					MkExtendedRegEx {
						bowAnchor	= maybeBowAnchor,
						concatenation	= concatenation',
						sternAnchor	= maybeSternAnchor
					},
					remainder
				) |
					(maybeBowAnchor, s1)		<- readsMaybeAnchor s,
					(concatenation', s2)		<- reads s1,
					(maybeSternAnchor, remainder)	<- readsMaybeAnchor s2
			 ] {-list-comprehension-} where
				readsMaybeAnchor :: ReadS (Maybe Anchor.Anchor)
				readsMaybeAnchor s'	= return {-to List-monad-} $ case reads s' of
					[pair]	-> Control.Arrow.first Just pair
					_	-> (Nothing, s')

-- | Shows either the specified 'Anchor.Anchor', or a null string where 'Nothing' is specified.
showsMaybeAnchor :: Maybe Anchor.Anchor -> String -> String
showsMaybeAnchor maybeAnchor
	| Data.Maybe.isJust maybeAnchor	= shows $ Data.Maybe.fromJust maybeAnchor
	| otherwise			= id

instance Show m => Show (ExtendedRegEx m)	where
	showsPrec _ MkExtendedRegEx {
		bowAnchor	= maybeBowAnchor,	-- CAVEAT: this could be 'Nothing' or perversely an 'Anchor.Stern'.
		concatenation	= concatenation',
		sternAnchor	= maybeSternAnchor	-- CAVEAT: this could be 'Nothing' or perversely an 'Anchor.Bow'.
	} = showsMaybeAnchor maybeBowAnchor . shows concatenation' . showsMaybeAnchor maybeSternAnchor

instance Consumer.Consumer (ExtendedRegEx m)	where
	consumptionProfile	= Consumer.aggregateConsumptionProfilesFromConcatenation . concatenation
	starHeight		= Consumer.starHeight . concatenation

instance ToolShed.SelfValidate.SelfValidator (ExtendedRegEx m)	where
	getErrors	= ToolShed.SelfValidate.getErrors . concatenation

instance Control.DeepSeq.NFData m => Control.DeepSeq.NFData (ExtendedRegEx m)	where
	rnf MkExtendedRegEx {
		bowAnchor	= maybeBowAnchor,
		concatenation	= concatenation',
		sternAnchor	= maybeSternAnchor
	} = Control.DeepSeq.rnf (maybeBowAnchor, concatenation', maybeSternAnchor)

-- | Drop 'Anchor.Anchor's at both bow & stern of the specified 'ExtendedRegEx'.
dock :: Transformation m
dock e	= e {
	bowAnchor	= Just Anchor.Bow,
	sternAnchor	= Just Anchor.Stern
}

-- | Amend the 'Concatenation' in the specified 'ExtendedRegEx', by prepending '.*' where there's a missing top-level 'Anchor.Bow' & appending '.*?' where there's a missing top-level 'Anchor.Stern'.
drift :: Transformation m
drift extendedRegEx	= transformExtendedRegEx (
	(
		if hasBowAnchor extendedRegEx then id else {-no anchor-} ((.*?) :)
	) . (
		if hasSternAnchor extendedRegEx then id else {-no anchor-} (++ [(.*)])
	)
 ) extendedRegEx

-- | True if the specified 'ExtendedRegEx' has an initial 'Anchor.Bow'. CAVEAT: though typically the alternative is no 'Anchor.Anchor', there may perversely be an initial 'Anchor.Stern', but neither qualify.
hasBowAnchor :: ExtendedRegEx m -> Bool
hasBowAnchor	= (== Just Anchor.Bow) . bowAnchor

-- | True if the specified 'ExtendedRegEx' has a terminal 'Anchor.Stern'. CAVEAT: though typically the alternative is no 'Anchor.Anchor', there may perversely be a terminal 'Anchor.Bow', but neither qualify.
hasSternAnchor :: ExtendedRegEx m -> Bool
hasSternAnchor	= (== Just Anchor.Stern) . sternAnchor

-- | True if there's at least one 'RepeatablePattern' in the 'Concatenation', i.e. that it's non-null.
isDefined :: ExtendedRegEx m -> Bool
isDefined	= not . null . concatenation

-- | Construct a null 'Match' by assuming that the specified 'ExtendedRegEx' consumes zero 'InputData'.
mkNullMatchFromExtendedRegEx :: ConsumptionBounds.DataLength -> ExtendedRegEx m -> MatchList m
mkNullMatchFromExtendedRegEx offset	= mkNullMatchFromConcatenation offset . concatenation

-- | The set of distinct (i.e. unique) 'Meta.Meta', in the specified 'ExtendedRegEx'.
getDistinctMetaDataFromExtendedRegEx :: Eq m => ExtendedRegEx m -> MetaDataList m
getDistinctMetaDataFromExtendedRegEx	= getDistinctMetaDataFromConcatenation . concatenation

{- |
	* The input-data is just a list.

	* Whilst typically this list is also a 'String', & could therefore be more efficiently implemented using "Data.ByteString",
	we can't assume that the polymorphic base-type is always 'Char'.
-}
type InputData m = [m]

-- | The accumulating sets of distinct input data.
accumulateDistinctInputData :: Eq m => InputData m -> [InputData m]
accumulateDistinctInputData	= scanr (Data.List.union . return) []

-- | Tag the 'InputData' with the 'RepeatablePattern' it matched (which unfortunately confines the definition to this (bloated) module), & the offset from the start of the data;
type MatchedData m	= (RepeatablePattern m, ConsumptionBounds.DataLength, InputData m)

-- | Shifts the offset of the specified 'MatchedData'.
shiftMatchedData
	:: ConsumptionBounds.DataLength	-- ^ The offset by which to shift the position into the input-data at which a match occurred.
	-> MatchedData m		-- ^ The match-structure whose offset is to be shifted.
	-> MatchedData m
shiftMatchedData i (r, offset, d)	= (r, offset + i, d)

-- | Accessor.
getInputData :: MatchedData m -> InputData m
getInputData (_, _, inputData)	= inputData

-- | Describes the manner in which a 'RepeatablePattern' successfully consumed 'InputData'.
type Match m	= Tree.Tree (MatchedData m)

-- | Shifts the offsets of all the 'MatchedData' contained in the specified 'Match'.
shiftMatch
	:: ConsumptionBounds.DataLength	-- ^ The offset by which to shift the position into the input-data at which a match occurred.
	-> Match m			-- ^ The match-structure whose offset is to be shifted.
	-> Match m
shiftMatch i	= (shiftMatchedData i `fmap`)

-- | Extract & concatenate, the 'InputData' from a 'Match'.
extractDataFromMatch :: Match m -> InputData m
extractDataFromMatch	= Data.Foldable.foldMap getInputData	-- Uses the List-monoid's associative binary operator (++), to concatenate the values returned by the specified function.

-- | Extract & concatenate, the 'InputData' from a 'Match'; null if it didn't match any.
extractDataFromMatch' :: Maybe (Match m) -> InputData m
extractDataFromMatch' (Just match)	= extractDataFromMatch match
extractDataFromMatch' _			= []

-- | Describes the manner in which a 'Concatenation' successfully consumed 'InputData'.
type MatchList m	= [Match m]

-- | Shifts the offsets of all the 'MatchedData' contained in the specified 'MatchList'.
shiftMatchList
	:: ConsumptionBounds.DataLength	-- ^ The offset by which to shift the position into the input-data at which a each listed match occurred.
	-> MatchList m			-- ^ The list of match-structures, each of whose offsets are to be shifted.
	-> MatchList m
shiftMatchList i	= map (shiftMatch i)

-- | Extract & concatenate, the 'InputData', from the 'MatchList'.
extractDataFromMatchList :: MatchList m -> InputData m
extractDataFromMatchList	= concatMap extractDataFromMatch	-- CAVEAT: too fine-grain for effective data-parallelism.

-- | At the top-level of an 'ExtendedRegEx', the lack of an 'Anchor.Anchor' allows the 'ExtendedRegEx' to drift away from the corresponding end of the input-data; this data-gap is captured here.
type ExternalMatch m	= Maybe (Match m)

-- | Captures the list of input-data consumed by the 'Concatenation', bracketed by any data-prefix or data-suffix.
type Result m	= (ExternalMatch m, Maybe (MatchList m), ExternalMatch m)

-- | Returns the length of data consumed by the specified 'ExternalMatch'.
externalMatchLength :: ExternalMatch m -> ConsumptionBounds.DataLength
externalMatchLength	= length . extractDataFromMatch'

{- |
	* Similar to '+~', but exposes 'ExecutionOptions.ExecutionOptions', permitting greater control; & enough rope to hang yourself.

	* The parameter-order facilitates partial application, to acquire a matcher-function for a specific 'ExtendedRegEx', which can be applied repeatedly to different 'InputData'.

	* Performs some one-off @O(n)@ time-complexity preparation, then delegates the grunt-work to a private recursive function.

	* One could waste a lot of time trying to consume 'InputData' from the head of a high-'Consumer.starHeight' 'ExtendedRegEx',
	only to find that there's a low-'Consumer.starHeight' tail that can't ever match the 'InputData'; checks this first before delegating.
-}
findMatch :: (Eq m, Control.DeepSeq.NFData m)
	=> RegExOpts.RegExOpts (ExtendedRegEx m)	-- ^ The match-options parameterised by the regex against which to match the input data.
	-> InputData m					-- ^ The input data within which to locate a match.
	-> Maybe (MatchList m)
findMatch regExOpts@RegExOpts.MkRegExOpts {
	RegExOpts.executionOptions	= executionOptions,
	RegExOpts.regEx			= extendedRegEx
} originalInputData
	| let
		extractLowStarHeightTail :: ExtendedRegEx m -> Concatenation m
		extractLowStarHeightTail	= {-#SCC "extractLowStarHeightTail" #-} fromConcatenation . reverse . concatenation	where
			fromConcatenation :: Concatenation m -> Concatenation m
			fromConcatenation []					= []
			fromConcatenation (repeatablePattern : concatenation')
				| Consumer.starHeight repeatablePattern == 0	= repeatablePattern : fromConcatenation {-recurse-} concatenation'
				| Repeatable.getFewest repeatablePattern > 0	= case Repeatable.base repeatablePattern of
					CaptureGroup alternatives
						| isSingletonAlternatives alternatives	-> extractLowStarHeightTail {-recurse-} . head $ deconstructAlternatives alternatives
						| otherwise				-> {-non-singleton-} []
					_				-> []	-- Zero 'Consumer.StarHeight' would have been detected by 'fromConcatenation'.
				| otherwise					= []

--		lowStarHeightTail :: Concatenation a
		lowStarHeightTail	= extractLowStarHeightTail extendedRegEx
	in and [
		ExecutionOptions.checkExistenceOfInelasticTail executionOptions,
		not $ null lowStarHeightTail,	-- Prevent infinite recursion.
		reverse originalInputData /~ RegExOpts.mkRegEx MkExtendedRegEx {
			bowAnchor	= Just Anchor.Bow,
			concatenation	= lowStarHeightTail,
			sternAnchor	= Nothing
		} -- Check for mismatch with the corresponding tail of 'InputData'.
	]		= Nothing
	| otherwise	= findMatchSlave originalConcatenation (
		Consumer.accumulateConsumptionProfiles originalConcatenation
	) (
		accumulateDistinctMetaDataFrom [] originalConcatenation
	) originalInputData originalInputDataLength (
		accumulateDistinctInputData originalInputData
	)
	where
--		originalConcatenation :: Concatenation a
		originalConcatenation	= concatenation extendedRegEx

		originalInputDataLength :: ConsumptionBounds.DataLength
		originalInputDataLength	= length originalInputData
{-
 This beast takes a lot of parameters.
 Whilst the only strictly necessary parameters are the lists 'Concatenation' & 'InputData', many ancillary lists derived from them, are also passed by parameter.
 These are proportionally reduced on recursion, to avoid the requirement to regenerate them each time they're required.
-}
		findMatchSlave :: (Eq m, Control.DeepSeq.NFData m)
			=> Concatenation m					-- The list of 'RepeatablePattern's from which the regex is constructed.
			-> ConsumptionProfile.AccumulatedConsumptionProfiles	-- The capacity-bounds for the consumption of 'InputData', that the 'Concatenation' extending right from any given 'RepeatablePattern', can consume.
			-> [MetaDataList m]					-- The set of distinct 'Meta.Meta'-data, in the 'Concatenation extending right from any given 'RepeatablePattern'.
			-> InputData m						-- The input data, which will be fed to the 'Concatenation'.
			-> ConsumptionBounds.DataLength				-- The length of the previously specified 'InputData'.
			-> [InputData m]					-- The set of distinct input data, extending right from any given point.
			-> Maybe (MatchList m)
		findMatchSlave [] _ _ [] _ _	= Just []	-- Simultaneous exhaustion of the 'Concatenation' of 'RepeatablePattern's & the 'InputData' => success.
		findMatchSlave [] _ _ _ _ _	= Nothing
		findMatchSlave concatenation'@(repeatablePatternHead : concatenationTail) (accumulatedConsumptionProfileHead : accumulatedConsumptionProfilesTail) (distinctMetaDataHead : distinctMetaDataTail) inputData inputDataLength distinctInputData@(distinctInputDataHead : _)
			| not $ inputDataLength `ConsumptionProfile.withinConsumptionBounds` accumulatedConsumptionProfileHead	= Nothing
			| null inputData	= Just $ mkNullMatchFromConcatenation inputDataOffset concatenation'	-- Build a 'Match', of null 'InputData'.
			| and [
				ExecutionOptions.bypassInputDataForLiberalConsumer executionOptions,			-- We may be able to establish success, without evaluating 'inputData' any further than required to determine its length.
				not $ ExecutionOptions.requireMatchList executionOptions,				-- Otherwise the precise mapping of the 'inputData' to 'RepeatablePattern's must be determined.
				not $ ConsumptionProfile.hasSpecificRequirement accumulatedConsumptionProfileHead	-- Otherwise a match for the specific 'Meta'-data must be found.
			] = Just undefined {-shouldn't be evaluated according to 'ExecutionOptions.requireMatchList'-}
			| and [
				ExecutionOptions.checkForUnconsumableData executionOptions,
				not $ ConsumptionProfile.canConsumeAnything accumulatedConsumptionProfileHead,	-- Otherwise the subsequent test will always fail.
				(`isUnconsumableByAnyOf` distinctMetaDataHead) `any` distinctInputDataHead	-- Occasionally failure is both inevitable & obvious.
			] = Nothing
			| otherwise	= {-#SCC "findMatchSlave" #-} let
				tailConsumptionProfile :: ConsumptionProfile.ConsumptionProfile
				tailConsumptionProfile@ConsumptionProfile.MkConsumptionProfile {
					ConsumptionProfile.consumptionBounds	= (minConsumptionConcatenationTail, maybeMaxConsumptionConcatenationTail)
				} = head accumulatedConsumptionProfilesTail	-- Extract the aggregate consumption-profile, of the tail of the 'Concatenation'.

				maxDataAvailable :: ConsumptionBounds.DataLength
				maxDataAvailable	= inputDataLength - minConsumptionConcatenationTail	-- The maximum data available to match 'repeatablePatternHead'.
			in if maxDataAvailable < 0
				then Nothing
				else {-inputData is sufficient for concatenationTail-} let
					Repeatable.MkRepeatable {
						Repeatable.base			= base,
						Repeatable.repetitionBounds	= (fewest, most),
						Repeatable.isGreedy		= isGreedy
					} = repeatablePatternHead	-- Completely deconstruct the 'Repeatable' at the head of the 'Concatenation'.
{-
 Find the maximum sequence of inputData, preceding the first of the mandatory requirements of 'concatenationTail'.
 This enables one to more tightly constrain the maximum number of repetitions of a 'RepeatablePattern',
 which consequently reduces the subsequent requirement to back-track.
-}
--					maximumDataBeforePegs :: Maybe (InputData m)
					maximumDataBeforePegs	= {-#SCC "maximumDataBeforePegs" #-} reverse (extractMinimumRequirementFromConcatenation concatenationTail) `maximumDataAfterPegs` reverse inputData	where
{-
 Extract those 'RepeatablePattern's which must match at least once.
 Singleton 'Alternatives' are flattened by replication, & non-singleton 'Alternatives' removed, allowing a reduction of the list's base-type to just 'Repeatable.Repeatable' 'Meta.Meta'.
 The resulting 'BasicRegEx', with /Kleene Star/s weeded-out & 'Alternatives' pruned, can be matched in O(n)-time, to verify minimal requirements.
-}
						extractMinimumRequirementFromConcatenation :: Concatenation m -> BasicRegEx m
						extractMinimumRequirementFromConcatenation	= concatMap (
							\repeatablePattern -> case Repeatable.base repeatablePattern of
								Require datum	-> return {-to List-monad-} repeatablePattern { Repeatable.base = datum }
								CaptureGroup a
									| isSingletonAlternatives a	-> concat . replicate (Repeatable.getFewest repeatablePattern) . extractMinimumRequirementFromConcatenation {-recurse-} . concatenation . head $ deconstructAlternatives a
									| otherwise			-> []
						 ) . filter ((> 0) . Repeatable.getFewest)

						maximumDataAfterPegs :: Eq m => BasicRegEx m -> InputData m -> Maybe (InputData m)
						maximumDataAfterPegs pegList@(
							Repeatable.MkRepeatable {
								Repeatable.base			= metaPeg,
								Repeatable.repetitionBounds	= (fewestPeg, _)
							} : pegListTail
						 ) inputData'
							| null remainingInputData && length candidateMatchedInputData < fewestPeg		= Nothing							-- Insufficient data to ever match.
							| fewestPeg == 1 || (`Meta.isMatch` metaPeg) `all` tail candidateMatchedInputData	= pegListTail `maximumDataAfterPegs` remainingInputData		-- Success => recurse.
							| otherwise										= pegList `maximumDataAfterPegs` tail candidateInputData	-- Failed candidate => recurse.
							where
--								candidateInputData, candidateMatchedInputData, remainingInputData :: InputData a
								candidateInputData				= dropWhile (not . (`Meta.isMatch` metaPeg)) inputData'
								(candidateMatchedInputData, remainingInputData)	= fewestPeg `splitAt` candidateInputData
						maximumDataAfterPegs [] inputData'	= Just inputData'	-- All the pegs have been matched.
{-
 Each 'repeatablePatternHead' can either be a 'Require' or a 'CaptureGroup'.
 The former case needs further reduction & we can proceed to construct a 'Tree.Leaf' from any matching 'inputData'.
 We attempt to reduce the latter case to an instance of the former, by expanding all combinations of repetitions of various 'Alternatives', constructing a new 'ExtendedRegEx' from each, & recursing.
-}
				in case base of
					Require meta
						| null maybeMatchList	-> Nothing
						| otherwise		-> head {-most suitable-} maybeMatchList
						where
{-
 Attempt to narrow the repetition-range, from the potentially unbounded specification, to that which is possible according to,
 the 'inputDataLength', & the specified consumption-bounds of the remainder of the /regex/.
 Once the repetition-range has been narrowed, iterate from the appropriate end of the range (depending on 'isGreedy'), extract the corresponding length from inputData, & check for a match.
-}
--							maybeMatchList :: [Maybe (MatchList m)]
							maybeMatchList	= {-#SCC "maybeMatchList" #-} dropWhile Data.Maybe.isNothing . map (
								\(consumedInputData, unconsumedInputData)	-> let
									consumedInputDataLength :: ConsumptionBounds.DataLength
									consumedInputDataLength	= length consumedInputData
								in (
									Tree.Leaf (repeatablePatternHead, inputDataOffset, consumedInputData) :
								) `fmap` {-apply to Maybe Functor-} findMatchSlave concatenationTail accumulatedConsumptionProfilesTail distinctMetaDataTail unconsumedInputData (
									inputDataLength - consumedInputDataLength
								) (
									consumedInputDataLength `drop` distinctInputData
								) -- Recurse, to check whether the 'unconsumedInputData' tail also matches.
							 ) $ let
								fewestData, mostData, mostData' :: Repeatable.Repetitions
								fewestData	= {-#SCC "fewestData" #-} case maybeMaxConsumptionConcatenationTail of
									Just m	-> (inputDataLength - m) {- :: ConsumptionBounds.DataLength-} `max` fewest {- :: Repeatable.Repetitions-}	-- CAVEAT: conceptually different types.
									_	-> fewest

--								mostData	= length . takeWhile (~= meta) $ (`take` inputData) maxData	-- As slow as it is concise.
								mostData	= {-#SCC "mostData" #-} maxData - measureUnmatchableTail inputData maxData	where
									maxData :: ConsumptionBounds.DataLength
									maxData	= case most of
										Just cap	-> cap {- :: Repeatable.Repetitions-} `min` maxDataAvailable {- :: ConsumptionBounds.DataLength-}	-- CAVEAT: conceptually different types.
										_		-> maxDataAvailable

--									measureUnmatchableTail :: InputData m -> ConsumptionBounds.DataLength -> ConsumptionBounds.DataLength
									measureUnmatchableTail _ 0	= 0						-- Have matched all the data.
									measureUnmatchableTail (a : as) unmatched
										| a `Meta.isMatch` meta	= measureUnmatchableTail as $ pred unmatched	-- Recurse.
										| otherwise		= unmatched					-- Return the unmatchable tail-length.
									measureUnmatchableTail [] _	= error "RegExDot.RegEx.findMatch.findMatchSlave.maybeMatchList.mostData.measureUnmatchableTail:\tdata unexpectedly exhausted."
{-
 'most' has been extracted from 'repeatablePatternHead',
 & reduced according to the length of 'inputData' which actually matches, to form 'mostData',
 but if it still exceeds 'fewestData', the resulting repetition-range may trigger backtracking.
 So, attempt to further reduce this ill-defined repetition-range, according to the competing requirements of 'concatenationTail'.
-}
								mostData'
									| and [
										ExecutionOptions.moderateGreed executionOptions,
										isGreedy,		-- Otherwise, since the search proceeds from 'fewest' to 'most', the optimal solution is located before backtracking.
										mostData > fewestData	-- Otherwise, there's no unbridled greed to moderate.
									] = case maximumDataBeforePegs of
										Just maximumDataBeforePegs'	-> mostData `min` length maximumDataBeforePegs'	-- Cap upper bound.
										_				-> negate 1	-- Guaranteed to be < 'fewestData'.
									| otherwise {-no requirement for this optimisation-}	= mostData
							 in (
								succ {-fence-post-} mostData' - fewestData	-- CAVEAT: possibly <= 0.
							 ) `take` (
								if isGreedy
									then ToolShed.Data.List.Splits.splitsLeftFrom mostData'
									else ToolShed.Data.List.Splits.splitsRightFrom fewestData
							 ) inputData

					CaptureGroup alternatives
						| and [
							not isGreedy,
							fewest <= 0,	-- Zero repetitions permissible.
							Data.Maybe.isJust tailMatch
						] -> zeroRepetitions	-- Zero repetitions is the optimal solution, rendering the choice of Alternative irrelevant.
						| and [
							ExecutionOptions.catchIncompatibleAnchors executionOptions,
							fewest > 1,			-- Multiple repetitions.
							minConsumptionAlternatives > 0,	-- InputData required.
							($ extendedRegExAlternatives) `any` [(hasBowAnchor `all`), (hasSternAnchor `all`)]
						]							-> Nothing	-- 'extendedRegExFromAlternative' requires 'InputData', therefore 'Anchor.Bow' can only pass on the 1st repetition & 'Anchor.Stern' can only pass on the last.
						| otherwise						-> let
{-
 We're about to try all repetitions of one Alternative before progressing to the next.
 One could alternatively transpose this search-pattern, & investigate all 'Alternatives' permissible at n repetitions, before trying (n + 1); I've not pursued this option.
 Either way, if the user requires data-capture, then for POSIX-compliance we must perform an exhaustive search of the O(Alternatives ^ Repetitions) permutations for the optimal solution; though the search may be narrowed as we proceed.
-}
--							matchPairList :: [(Match m, MatchList m)]
							matchPairList	= {-#SCC "matchPairList" #-} Data.Maybe.catMaybes . concat . Control.Parallel.Strategies.parMap Control.Parallel.Strategies.rseq {-Particularly effective when 'not ExecutionOptions.requireMatchList' -} (
--								:: ExtendedRegEx m -> [Maybe (Match m, MatchList m)]
								\extendedRegExFromAlternative@MkExtendedRegEx {
									concatenation	= concatenationFromAlternative
								} -> let
									components :: Int
									components	= length concatenationFromAlternative

									consumptionProfileConcatenationFromAlternative :: [ConsumptionProfile.ConsumptionProfile]
									consumptionProfileConcatenationFromAlternative	= map Consumer.consumptionProfile concatenationFromAlternative

									minConsumptionConcatenationFromAlternative	:: ConsumptionBounds.DataLength
									maybeMaxConsumptionConcatenationFromAlternative	:: Maybe ConsumptionBounds.DataLength
									(minConsumptionConcatenationFromAlternative, maybeMaxConsumptionConcatenationFromAlternative)	= ConsumptionProfile.consumptionBounds $ ConsumptionProfile.aggregateFromConcatenation consumptionProfileConcatenationFromAlternative
								in (
									if ExecutionOptions.abortTrialRepetitionsOnZeroConsumption executionOptions && Data.Maybe.isNothing most {-constrained only by 'inputDataLength'-}
										then {-#SCC "abortTrialRepetitionsOnZeroConsumption" #-} ToolShed.Data.List.takeUntil $ \maybeMatchPair -> case maybeMatchPair of
											Just (match, _)	-> ($ Tree.pop match) `all` [
												(>= fewest) . length,			-- Must achieve 'Repeatable.getFewest', regardless of data-consumption.
												null . extractDataFromMatchList . last	-- If the n-th repetition consumed nothing, so will the (n + 1)-th.
											 ]
											_		-> False			-- Failure with n repetitions, doesn't preclude success with n + 1.
										else {-optimisation not required-} id
								) . (
--									:: Maybe (Match m, MatchList m) -> Maybe (Match m, MatchList m)
									\maybeMatchPairList	-> if ExecutionOptions.abortTrialRepetitionsOnInherentFailure executionOptions
{-
 We're iterating through the identified range of possible repetitions, of one possible 'concatenationFromAlternative'.
 If failure can be proven, for an arbitrary number of repetitions, then we can bypass the futile remainder.
 If the first trial succeeded, then clearly there's no such inherent problem with 'concatenationFromAlternative', & it's necessary to evaluate all subsequent trial-repetitions.
 Otherwise, assuming there're subsequent trials to be bypassed, attempt to prove that failure is inevitable, by launching a pilot-study,
 using a single repetition of 'concatenationFromAlternative' followed by a replacement infallible 'concatenationTail'.
-}
										then {-#SCC "abortTrialRepetitionsOnInherentFailure" #-} case maybeMatchPairList of
											Nothing {-failed attempt-} : _ {-subsequent attempt worth bypassing-} : _	-> if (
												minConsumptionConcatenationFromAlternative == 0	-- Can delegate consumption of unconsumable data to the infallible tail => unprovable culpability.
											 ) || inputData =~ RegExOpts.mkRegEx MkExtendedRegEx {
												bowAnchor	= Just Anchor.Bow,
												concatenation	= concatenationFromAlternative ++ [anyDatum ^#-> minConsumptionConcatenationTail],	-- Infallible tail.
												sternAnchor	= Just Anchor.Stern
											 }
												then maybeMatchPairList
												else []	-- Abandon further repetitions of 'Alternatives'.
											_ {-Alternative is OK, or there's no subsequent attempt to bypass-}		-> maybeMatchPairList
										else {-optimisation not required-} maybeMatchPairList
								) . map (
--									:: Repeatable.Repetitions -> Maybe (Match m, MatchList m)
									\repetitions	-> if hasSternAnchor extendedRegExFromAlternative
										then if (repetitions == 1 || minConsumptionConcatenationFromAlternative == 0) && minConsumptionConcatenationTail == 0
											then (
												shiftMatch inputDataOffset . Tree.Node . (
													: replicate (
														pred repetitions	-- Typically degenerates to zero, since anchored sub-expressions aren't normally repeatable.
													) (
														mkNullMatchFromExtendedRegEx 0 extendedRegExFromAlternative	-- Other Alternatives may be available & suitable.
													) -- Repeated null match may, lacking other suitable Alternatives, correspond to this stern-anchored ExtendedRegEx, so data must be consumed before them.
												) &&& const (mkNullMatchFromConcatenation inputDataOffset concatenationTail)
											) `fmap` findMatch regExOpts { RegExOpts.regEx = extendedRegExFromAlternative } inputData	-- Recurse, either consuming all inputData or failing.
											else Nothing
										else {-no Anchor.Stern-} let
{-
 Expand the 'Repeatable CaptureGroup', so that after recursion it is ultimately reduced in complexity, to just a 'Concatenation' of 'Repeatable Require', & can be handled above as a POSIX BRE.

 There're two special cases; one repetition of n 'Alternatives' & n repetitions of one Alternative.
 If there's only one repetition, we can reduce the implementation-logic substantially, to the point where the implementation's independent of whether there's a choice of 'Alternatives'.
 If there only one Alternative, then the entire repetition-sequence can be immediately expanded, vastly reducing the required number of recursions,
 otherwise we can only expand the first repetition, leaving the option of matching a different Alternative on subsequent repetitions.

 In all cases, we must subsequently re-compose any 'MatchList' resulting of the recursive call, into one corresponding to the structure of original ExtendedRegEx.
 The expansion-process & corresponding subsequent collation, are associated in a tuple, in which the consumption-profile of the expansion is also contained.
-}
--											collater					:: MatchList m -> (Match m, MatchList m)
											consumptionProfileExpandedConcatenationPrefix	:: [ConsumptionProfile.ConsumptionProfile]
--											expandedConcatenationPrefix			:: Concatenation a
											(collater, consumptionProfileExpandedConcatenationPrefix, expandedConcatenationPrefix)
												| repetitions == 1	= (
													Control.Arrow.first (
														Tree.Node . return {-to List-monad-}
													) . splitAt components,	-- Bisect the 'MatchList', into the part resulting from the Alternative, & that for 'concatenationTail'.
													consumptionProfileConcatenationFromAlternative,
													concatenationFromAlternative
												) -- Both tuples below, can degenerate to this simple case; it isn't a fundamentally different algorithm.
												| and [
													ExecutionOptions.unrollRepeatedSingletonAlternative executionOptions,
													isSingletonAlternative,
													not $ hasBowAnchor extendedRegExFromAlternative
												] = (
													Control.Arrow.first (
														Tree.Node . ToolShed.Data.List.chunk components
													) . splitAt (
														components * repetitions
													), -- Bisect the 'MatchList', into the part resulting from (Alternative){n}, & that for 'concatenationTail'.
													concat $ replicate repetitions consumptionProfileConcatenationFromAlternative,
													concat $ replicate repetitions concatenationFromAlternative	-- Expand all repetitions of the single alternative.
												)
												| otherwise {-choice of Alternatives or has Anchor.Bow-}	=  let
													remainingRepetitions :: Repeatable.Repetitions
													remainingRepetitions	= pred repetitions	-- Expand just the first repetition of the set of 'Alternatives'.

--													singletonRepeatable :: RepeatablePattern a
													singletonRepeatable	= Repeatable.toSingleton repeatablePatternHead
												in (
													Control.Arrow.first (
														Tree.Node . uncurry (:) . Control.Arrow.second (
															concatMap Tree.pop		-- Amalgamate the repeated singleton 'MatchList's.
														) . splitAt components			-- Bisect first 'MatchList', into the part resulting from 'concatenationFromAlternative', & that for 'replicate remainingRepetitions'.
													) . splitAt (
														components + remainingRepetitions	-- Bisect the 'MatchList', into the part resulting from (Alternatives){n}, & that for 'concatenationTail'.
													),
													consumptionProfileConcatenationFromAlternative ++ replicate remainingRepetitions (Consumer.consumptionProfile singletonRepeatable),
													concatenationFromAlternative {-expand 1st repetition-} ++ replicate remainingRepetitions {-potentially zero-} singletonRepeatable	-- Enumerate all remaining repetitions of ANY Alternative; a different Alternative can match for each repetition.
												)
									in {-#SCC "collater" #-} collater `fmap` {-apply to Maybe Functor-} findMatchSlave (
										expandedConcatenationPrefix ++ concatenationTail
									) (
										ConsumptionProfile.accumulateFrom tailConsumptionProfile {-initial value-} consumptionProfileExpandedConcatenationPrefix ++ tail accumulatedConsumptionProfilesTail
									) (
										accumulateDistinctMetaDataFrom (head distinctMetaDataTail) {-initial value-} expandedConcatenationPrefix ++ tail distinctMetaDataTail
									) inputData inputDataLength distinctInputData	-- Recurse to get 'Maybe (MatchList a)'.
								) $ if Repeatable.isPrecise repeatablePatternHead	-- These are frequently generated by the previous recursion, as a trial expansion of an Alternative.
									then if ExecutionOptions.validateMinConsumptionOfAlternatives executionOptions && maxDataAvailable < fewest * minConsumptionAlternatives
										then []	-- Failure is inevitable.
										else [fewest]
									else {-imprecise range-} let
{-
 Attempt to narrow the repetition-range, from the potentially unbounded specification, to that which is possible according to (data available / consumption-requirement),
 where there're bounds for both numerator & denominator.
 Additionally, one must account for the possibility of the denominator being zero, & if so, select the optimal value if the numerator is also zero.
 The range can be narrowed more aggressively, for the special case of a singleton 'Alternative', as used in a capture-group.
-}
										fewestAlternatives, mostAlternatives, mostAlternatives' :: Repeatable.Repetitions
										fewestAlternatives	= {-#SCC "fewestAlternatives" #-} 1 {-zero is handled later-} `max` if any Data.Maybe.isNothing [maybeMaxConsumptionConcatenationTail, maybeMaxConsumptionAlternatives]
											then fewest
											else {-no unlimited capacities-} let
												minDataAvailable, maxConsumptionAlternatives :: ConsumptionBounds.DataLength
												minDataAvailable		= inputDataLength - Data.Maybe.fromJust maybeMaxConsumptionConcatenationTail	-- CAVEAT: can be negative.
												maxConsumptionAlternatives	= Data.Maybe.fromJust maybeMaxConsumptionAlternatives
											in if isSingletonAlternative
												then if maxConsumptionAlternatives == 0							-- Denominator.
													then {-sole Alternative can't consume anything ?!-} if minDataAvailable <= 0	-- Numerator.
														then {-tail may consume all data-} fewest	-- Repeat to meet the minimum requirement.
														else {-unconsumable data-} maxBound		-- Failure is inevitable.
													else {-non-zero => can divide-} max fewest $ minDataAvailable /+ maxConsumptionAlternatives
												else {-choice of Alternatives-} let
													minDataAvailable' :: ConsumptionBounds.DataLength
													minDataAvailable'	= minDataAvailable - Data.Maybe.fromJust maybeMaxConsumptionConcatenationFromAlternative	-- CAVEAT: potentially negative.
												in if maxConsumptionAlternatives == 0						-- Denominator.
													then {-no Alternative can consume anything-} if minDataAvailable' <= 0	-- Numerator.
														then {-zero unconsumable data-} fewest	-- Repeat to meet the minimum requirement.
														else {-unconsumable data-} maxBound	-- Failure is inevitable.
													else {-non-zero => can divide-} max fewest . succ {-account for expanded instance-} $ minDataAvailable' /+ maxConsumptionAlternatives

										mostAlternatives	= {-#SCC "mostAlternatives" #-} case most of
											Just cap	-> cap `min` mostPermissibleRepetitions	-- Create a ceiling above which the calculated number of permissible repetitions can't rise.
											_		-> mostPermissibleRepetitions
											where
												mostPermissibleRepetitions :: Repeatable.Repetitions
												mostPermissibleRepetitions
													| isSingletonAlternative		= if minConsumptionAlternatives == 0							-- Denominator.
														then {-sole Alternative can consume zero data-} if maxDataAvailable == 0	-- Numerator.
															then {-zero data available-} fewest		-- Repeat to meet the minimum requirement.
															else {-data available-} maxDataAvailable	-- Either each repetition consumes something, or (n - 1) repetitions is a better solution.
														else {-non-zero => can divide-} maxDataAvailable `div` minConsumptionAlternatives	-- Divide & round down.
													| otherwise {-choice of Alternatives-}	= 1 {-account for instance expanded as 'concatenationFromAlternative'-} + let
														maxDataAvailable' :: ConsumptionBounds.DataLength
														maxDataAvailable'	= maxDataAvailable - minConsumptionConcatenationFromAlternative	-- CAVEAT: potentially negative.
													in if minConsumptionAlternatives == 0							-- Denominator.
														then {-@ least one Alternative can consume zero-} if maxDataAvailable' == 0	-- Numerator.
															then {-zero data available $-} pred fewestAlternatives	-- Repeat as required. CAVEAT: an annoying dependency, which prevents parallel-evaluation.
															else {-data available-} if and [
																False,	-- The cost outweighs the small infrequent dividend.
																minConsumptionConcatenationFromAlternative == 0,
																maxDataAvailable > fewest
															] -- If 'fewest' can be met, without unconsuming repetitions of 'concatenationFromAlternative', then stop short of permitting it.
																then pred maxDataAvailable	-- Any greater, & one repetition (possibly that currently expanded) must needlessly consume nothing.
																else maxDataAvailable'		-- Either all n repetitions consumes something, or (n - 1) is a better solution.
														else {-non-zero => can divide-} maxDataAvailable' `div` minConsumptionAlternatives	-- Divide & round down.
{-
 'most' has been extracted from 'repeatablePatternHead',
 & reduced according to the number of times it can fit into the maximum available data, to form 'mostAlternatives',
 but if it still exceeds 'fewestAlternatives', the resulting repetition-range may trigger backtracking.
 So, attempt to further reduce this ill-defined repetition-range, according to the competing requirements of 'concatenationTail'.
 CAVEAT: this pays-off infrequently, but not necessarily insignificantly.
-}
										mostAlternatives'	= {-#SCC "mostAlternatives'" #-} if and [
											ExecutionOptions.moderateGreed executionOptions,
											mostAlternatives > fewestAlternatives,	-- Otherwise, there's no unbridled greed to moderate.
											minConsumptionAlternatives > 0		-- Otherwise, any number of repetitions can occur without necessarily triggering back-tracking.
										 ]
											then case maximumDataBeforePegs of
												Just maximumDataBeforePegs'	-> mostAlternatives `min` (
													length maximumDataBeforePegs' `div` minConsumptionAlternatives
												 ) -- Cap upper bound.
												_				-> negate 1	-- Guaranteed to be < 'fewestAlternatives'.
											else {-no requirement for this optimisation-} mostAlternatives
									in [fewestAlternatives .. mostAlternatives']	-- The permissible repetition-range of any Alternative, constrained by the amount of data available, after subtracting fixed consumption-requirements.
							 ) . (
								if ExecutionOptions.permitReorderingOfAlternatives executionOptions
{-
 The order in which 'Alternatives' are evaluated, should make no difference to the final result; unless 'ExecutionOptions.useFirstMatchAmongAlternatives'.
 This order can be manipulated in an attempt to more quickly locate any 'Just MatchList', though if 'ExecutionOptions.requireMatchList', an exhaustive search for the optimal solution must be performed anyway.
 Tackling lower-complexity 'Alternatives' first makes sense, in the hope of finding any easy solution before becoming entrenched fighting some hideous monster.
 Preferring 'Alternatives' which require more data also makes sense, regardless of 'Repeatable.isGreedy' which just a preference for a type of solution, because fewer repetitions are then permissible;
 this is rather like trying to pack a box & preferring to try the biggest items first.
-}
									then Data.List.sortBy $ Data.Ord.comparing (
										Consumer.starHeight &&& safeReciprocal . (fromIntegral :: ConsumptionBounds.DataLength -> Rational) . Consumer.getFewest
									) -- Firstly increasing complexity, then decreasing minimum data-capacity, otherwise stable.
									else id
							 ) $ filter (
								\e	-> (
									not (hasBowAnchor e) || inputDataOffset == 0			-- Necessary & sufficient.
								) && (
									not (hasSternAnchor e) || minConsumptionConcatenationTail == 0	-- Necessary but insufficient, since though minimum consumption is zero, maximum isn't necessarily.
								)
							 ) extendedRegExAlternatives
						in if null matchPairList
							then if fewest <= 0
								then zeroRepetitions	-- Which might still fail, depending on 'tailMatch'.
								else {-zero repetitions isn't permissible-} Nothing
							else {-at least one Alternative matched-} {-#SCC "selectAlternative" #-} Just . uncurry (:) {-re-join head & tail-} $ (
{-
 At least one 'Match' has been found amongst the combinations of repetitions of various 'Alternatives',
 & if the caller reduces the 'Just MatchList' to 'True', that'd be sufficient, but otherwise one must identify the optimal 'MatchList'.
 The primary selection-criterion used is the POSIX one, based on total maximum (or minimum for non-'Repeatable.isGreedy' capture-groups) data-consumption.
 /Perl/'s "first-past-the-post" policy is available as an alternative.
-}
								if ExecutionOptions.useFirstMatchAmongAlternatives executionOptions || length matchPairList == 1
									then head
									else snd {-remove prepended selection-criterion-} . Data.List.maximumBy {-select the best match-} (
										Data.Ord.comparing fst	-- Compare using only the criterion, not the result from which it was derived.
									) . map (
										(
{-
 If the primary selection-criterion doesn't resolve the choice between candidate 'Match'es, I employ these ad-hoc criteria.

	* Fewer repetitions are preferred, which discourages the capture of null lists of 'InputData'.

	* Within the 'MatchedData', from which a candidate 'Match' is ultimately composed, consumption of 'InputData' beyond 'Repeatable.getFewest' by 'Repeatable.isGreedy' 'RepeatablePattern's, is preferred to non-greedy ones.

	* The consumption on each successive repetition is compared between candidate 'Match'es (which are now known to have used an equal number of repetitions);
	this causes the data-consumption to flow towards earlier repetitions for greedy capture-groups, & towards later repetitions in non-greedy ones,
	thus mimicking the behaviour of the unrolled repetition.

	* PS: more precise criteria are defined in <http://www2.research.att.com/~gsf/testregex/re-interpretation.html>.
-}
											\match -> let
												dataLengthCriterion :: InputData m -> Rational
												dataLengthCriterion	= (
													if isGreedy
														then id
														else safeReciprocal	-- Prefer less data.
												 ) . fromIntegral . length

--												matchLists :: [MatchList m]
												matchLists	= Tree.pop match
											in (
												dataLengthCriterion $ extractDataFromMatch match,
												if ExecutionOptions.preferFewerRepeatedAlternatives executionOptions
													then safeReciprocal . toRational $ length matchLists
													else 0,
												if ExecutionOptions.preferAlternativesWhichFeedTheGreedy executionOptions
													then let
														excessConsumption :: MatchedData m -> ConsumptionBounds.DataLength
														excessConsumption (
															Repeatable.MkRepeatable {
																Repeatable.repetitionBounds	= (f, _),
																Repeatable.isGreedy		= g
															},
															_,
															consumedInputData
														 )
															| g		= l - f
															| otherwise	= f - l
															where
																l :: ConsumptionBounds.DataLength
																l	= length consumedInputData
													in Data.Foldable.foldl' (\l -> (l +) . excessConsumption) 0 match
													else 0,
												if ExecutionOptions.preferAlternativesWhichMimickUnrolling executionOptions
													then map (dataLengthCriterion . extractDataFromMatchList) matchLists
													else []
											) -- Create a tuple of selection-criteria, for simultaneous assessment.
										) . fst {-focus the choice on the Alternatives-} &&& id	-- Prepend a selection-criterion to each result.
									)
							) matchPairList
						where
--							tailMatch, zeroRepetitions :: Maybe (MatchList m)
							tailMatch	= findMatchSlave concatenationTail accumulatedConsumptionProfilesTail distinctMetaDataTail inputData inputDataLength distinctInputData	-- Recurse.
							zeroRepetitions	= (Tree.Node [] :) `fmap` {-apply to Maybe Functor-} tailMatch	-- Prepend a null 'MatchList'.

--							extendedRegExAlternatives :: [ExtendedRegEx m]
							extendedRegExAlternatives	= Data.List.nub $ deconstructAlternatives alternatives

							isSingletonAlternative :: Bool
							isSingletonAlternative	= length extendedRegExAlternatives == 1

							minConsumptionAlternatives	:: ConsumptionBounds.DataLength
							maybeMaxConsumptionAlternatives	:: Maybe ConsumptionBounds.DataLength
							(minConsumptionAlternatives, maybeMaxConsumptionAlternatives)	= Consumer.getConsumptionBounds alternatives	-- Independent of the choice of Alternative.
			where
				inputDataOffset :: ConsumptionBounds.DataLength
				inputDataOffset	= originalInputDataLength - inputDataLength
		findMatchSlave _ _ _ _ _ _	= error "RegExDot.RegEx.findMatch.findMatchSlave:\tunexpected parameter-pattern."

{- |
	* Operator's name was chosen to suggest something more than '=~'.

	* CAVEAT: much more expensive then '=~': in /ghci/, 'Just' can be observed to be printed /long/ before the 'MatchList' from which 'Result' is constructed,
	as the lazy algorithm finds the first solution, but not yet necessarily the optimal solution, amongst 'Alternatives'.
-}
(+~) :: (Eq m, Control.DeepSeq.NFData m)
	=> InputData m					-- ^ The input data within which to locate a match.
	-> RegExOpts.RegExOpts (ExtendedRegEx m)	-- ^ The match-options parameterised by the regex against which to match the input data.
	-> Result m
inputData +~ regExOpts	= (
	if hasBowAnchor' then Nothing else fmap head maybeMatchList,	-- Record the first 'Match', consumed in the absence of a top-level 'Anchor.Bow'.
	fmap (
		(
			if hasBowAnchor' then id else tail			-- Remove the first 'Match', since this wasn't consumed by the 'Concatenation'.
		) . (
			if hasSternAnchor' then id else init			-- Remove the last 'Match', since this wasn't consumed by the 'Concatenation'.
		)
	) maybeMatchList,
	if hasSternAnchor' then Nothing else fmap last maybeMatchList	-- Record the last 'Match', consumed in the absence of a top-level 'Anchor.Stern'.
 ) where
--	extendedRegEx :: ExtendedRegEx a
	extendedRegEx	= RegExOpts.regEx regExOpts

	hasBowAnchor', hasSternAnchor' :: Bool
	hasBowAnchor'	= hasBowAnchor extendedRegEx
	hasSternAnchor'	= hasSternAnchor extendedRegEx

--	maybeMatchList :: Maybe (MatchList m)
	maybeMatchList	= fmap drift (RegExOpts.setVerbose True regExOpts) `findMatch` inputData

{- |
	* Pattern-match operator.

	* Identifier & parameter-order follow the lead of /Perl/'s pattern-match operator.

	* Considerably more efficient than '+~', since even though they are both implemented via 'findMatch',
	the discovery of /any/ solution is sufficient to generate the return-value;
	lazy-evaluation avoids the requirement to identify the irrelevant optimal solution.
-}
(=~) :: (Eq m, Control.DeepSeq.NFData m)
	=> InputData m					-- ^ The input data within which to locate a match.
	-> RegExOpts.RegExOpts (ExtendedRegEx m)	-- ^ The match-options parameterised by the regex against which to match the input data.
	-> Bool
inputData =~ regExOpts	= Data.Maybe.isJust $ fmap drift (RegExOpts.setVerbose False regExOpts) `findMatch` inputData

-- | Pattern-mismatch operator.
(/~) :: (Eq m, Control.DeepSeq.NFData m)
	=> InputData m					-- ^ The input data within which to locate a match.
	-> RegExOpts.RegExOpts (ExtendedRegEx m)	-- ^ The match-options parameterised by the regex against which to match the input data.
	-> Bool
(/~) inputData	= not . (inputData =~)

-- | The delimiters of 'Alternatives', when in the 'String'-form.
captureGroupDelimiters :: (Char, Char)
captureGroupDelimiters	= ('(', ')')

-- | The token used to separate alternative 'ExtendedRegEx's, when in the 'String'-form.
alternativeExtendedRegExSeparatorToken :: Char
alternativeExtendedRegExSeparatorToken	= '|'

-- | The set of 'Char' to which a specific meaning is attributed, when reading from 'String'.
tokens :: String
tokens	= [fst captureGroupDelimiters, snd captureGroupDelimiters, alternativeExtendedRegExSeparatorToken]

{- |
	* Invert the specified fraction, but return 'maxBound' if the result would otherwise be infinite.

	* CAVEAT: this is an awful concept, and therefore intended for internal use only.
-}
safeReciprocal :: (Eq f, Fractional f) => f -> f
safeReciprocal 0	= fromIntegral (maxBound :: Int)	-- Handle divide-by-zero error.
safeReciprocal f	= recip f

infixl 7 /+	-- Same as (/).

{- |
	* Integral division, with any fractional remainder rounded-up.

	* A rather dubious requirement, so internal use only.
-}
-- (/+) :: Integral i => i -> i -> i
(/+)
	:: Int	-- ^ Numerator.
	-> Int	-- ^ Denominator.
	-> Int	-- 10% faster in unoptimised code, & more in optimised.
_ /+ 0				= error "RegExDot.RegEx.(/+):\tzero denominator => infinity"
-- numerator /+ denominator	= ceiling ((fromIntegral numerator / fromIntegral denominator) :: Double)
numerator /+ denominator	= uncurry (+) . Control.Arrow.second signum $ quotRem numerator denominator	-- Slightly faster.
