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

	* Describes the set of polymorphic data, which can be matched.

	* Only permits a match against exactly one polymorphic datum, which distinguishes it from a /zero-width assertion/, like an /anchor/, /word-boundary/, or /look-ahead assertion/.

	* Designed to be used by a polymorphic /regex/-engine, to implement the traditional meta-characters; @. [] [^]@.

	* Permits /Perl-style shortcuts/ for commonly used 'Meta'-data, to be canned & assigned a single-'Char' mnemonic for subsequent reference;
	the implementation of 'Read' looks for a back-slashed 'Char', for which it expects there to be a corresponding canned 'Meta'.
	Since this class is polymorphic, it has no knowledge of what shortcuts might be appropriate for the chosen type-parameter,
	so the expansion from the back-slashed 'Char' to corresponding 'Meta'-data, is performed through the 'expand' interface of the 'ShortcutExpander' class, which should be implemented elsewhere.
-}

module RegExDot.Meta(
-- * Type-classes
	ShortcutExpander(..),
-- * Types
-- ** Data-types
	Meta(..),
-- * Constants
	shortcutToken,
	anyToken,
	tokens,
-- * Functions
-- ** Predicates
	isMatch
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	RegExDot.BracketExpression		as BracketExpression
import qualified	RegExDot.BracketExpressionMember	as BracketExpressionMember
import qualified	RegExDot.Consumer			as Consumer
import qualified	RegExDot.ConsumptionProfile		as ConsumptionProfile
import qualified	RegExDot.ShowablePredicate		as ShowablePredicate
import qualified	ToolShed.SelfValidate

{- |
	* The interface via which /Perl-style shortcut/s are expanded, in a manner appropriate to the chosen type-parameter.

	* Since the expansion of /Perl-style shortcut/s, is more restricted inside than outside a 'BracketExpression.BracketExpression',
	the former is considered to be a superclass, providing a base from which to build alternative implementations.
-}
class BracketExpressionMember.ShortcutExpander m => ShortcutExpander m	where
	expand	:: Char -> Meta m	-- ^ Expand a /Perl-style shortcut/.

-- | Declares a polymorphic data-type.
data Meta m =
	Any							-- ^ Any datum matches. Equivalent to @NoneOf []@, but more efficient. CAVEAT: independent of the type-parameter @a@.
	| Literal m						-- ^ The datum matches, if it's equal to the specified value. Equivalent to @AnyOf [BracketExpression.Literal x]@, but more efficient.
	| AnyOf (BracketExpression.BracketExpression m)		-- ^ The datum matches, if 'BracketExpression.containsMatch'.
	| NoneOf (BracketExpression.BracketExpression m)	-- ^ The datum matches, if @not BracketExpression.containsMatch@.
	| Predicate (ShowablePredicate.ShowablePredicate m)	-- ^ The datum matches if 'ShowablePredicate.ShowablePredicate'.
	deriving (
		Eq
--		Read,	-- Specialised below.
--		Show	-- Specialised below.
	)

instance ToolShed.SelfValidate.SelfValidator (Meta m)	where
	getErrors _	= []

instance Show m => Show (Meta m)	where
	showsPrec _ Any					= showChar anyToken
	showsPrec _ (Literal m)				= shows m
	showsPrec _ (AnyOf bracketExpression)		= shows bracketExpression
	showsPrec _ (NoneOf bracketExpression)		= showChar x . showChar BracketExpression.negationToken . showString xs	where (x : xs)	= show $ AnyOf bracketExpression
	showsPrec _ (Predicate showablePredicate)	= shows showablePredicate

instance (ShortcutExpander m, Read m) => Read (Meta m)	where
	readsPrec _ []				= []		-- No parse.
	readsPrec _ (' ' : s)			= reads s	-- Consume white-space.
	readsPrec _ ('\t' : s)			= reads s	-- Consume white-space.
	readsPrec _ ('.' : s)			= [(Any, s)]
	readsPrec _ ('[' : '^' : noneOf)	= Control.Arrow.first NoneOf `map` reads (fst BracketExpression.delimiterTokens : noneOf) {-Reconstruct without negation, & recurse-}
	readsPrec _ anyOf@('[' : _)		= Control.Arrow.first AnyOf `map` reads anyOf {-singleton-}
	readsPrec _ ('\\' : c : s)		= [(expand c, s)]
	readsPrec _ literal			= Control.Arrow.first Literal `map` reads literal {-singleton-}

instance Consumer.Consumer (Meta m)	where
	consumptionProfile meta	= let
		hasSpecificDataRequirement :: Bool
		hasSpecificDataRequirement	= case meta of
			Any		-> False
			NoneOf []	-> False
			_		-> True
	 in ConsumptionProfile.MkConsumptionProfile {
		ConsumptionProfile.consumptionBounds		= (1, Just 1),
		ConsumptionProfile.hasSpecificRequirement	= hasSpecificDataRequirement,
		ConsumptionProfile.canConsumeAnything		= not hasSpecificDataRequirement
	 }

	starHeight _	= 0

instance Control.DeepSeq.NFData m => Control.DeepSeq.NFData (Meta m)	where
	rnf Any					= ()
	rnf (Literal m)				= Control.DeepSeq.rnf m
	rnf (AnyOf bracketExpression)		= Control.DeepSeq.rnf bracketExpression
	rnf (NoneOf bracketExpression)		= Control.DeepSeq.rnf bracketExpression
	rnf (Predicate showablePredicate)	= Control.DeepSeq.rnf showablePredicate

-- | True if the specified datum matches.
isMatch :: Eq m
	=> m		-- ^ The input datum.
	-> Meta m	-- ^ The meta-entity against which the input datum is to be matched.
	-> Bool		-- ^ The result of the match-operation.
isMatch _ Any					= True
isMatch datum (Literal literal)			= datum == literal
isMatch datum (AnyOf bracketExpression)		= datum `BracketExpression.containsMatch` bracketExpression
isMatch datum (NoneOf bracketExpression)	= not $ datum `isMatch` AnyOf bracketExpression	-- This implementation leverages future enhancements to 'AnyOf'.
isMatch datum (Predicate showablePredicate)	= ShowablePredicate.predicate showablePredicate datum

-- | The token used to precede a /Perl-style shortcut/, when in the 'String'-form.
shortcutToken :: Char
shortcutToken	= '\\'

-- | The token used to denote 'Any', when in the 'String'-form.
anyToken :: Char
anyToken	= '.'

-- | The set of 'Char' to which a specific meaning is attributed, when reading from 'String'.
tokens :: String
tokens	= [shortcutToken, anyToken] ++ BracketExpression.tokens

