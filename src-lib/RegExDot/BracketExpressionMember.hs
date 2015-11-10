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

	Permits /Perl-style shortcut/s to be canned & assigned a single-'Char' mnemonic for subsequent reference;
	the implementation of 'Read' looks for a back-slashed 'Char', for which it expects there to be a corresponding canned 'ShowablePredicate.ShowablePredicate'.

 [@CAVEATS@]

	Since the underlying polymorphic data-type isn't required to implement neither 'Enum' nor 'Ord', the implementation of 'Read' can't cope with range-specifications.
	Lacking this, Bracket-expression members must be enumerated exhaustively.
-}

module RegExDot.BracketExpressionMember(
-- * Type-classes
	ShortcutExpander(..),
-- * Types
-- ** Data-types
	Member(..),
-- * Functions
-- ** Operators
	(=~)
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	RegExDot.ShowablePredicate	as ShowablePredicate

infix 4 =~	-- Same as (==).

{- |
	* The interface via which /Perl-style shortcut/s are expanded (when they occur within a /bracket-expression/), in a manner appropriate to the chosen type-parameter.

	* The expansion of /Perl-style shortcut/s, is more restricted inside than outside, a /bracket-expression/,
	& consequently are merely represented here by a 'ShowablePredicate.ShowablePredicate', rather than providing a more general form suitable also for those /Perl-style shortcuts/ found outside /bracket-expression/s.

	* This interface is implemented elsewhere, where the specific type-parameter & consequently the appropriate set of /Perl-style shortcut/s, are defined.
-}
class ShortcutExpander a	where
	findPredicate	:: Char -> Maybe (ShowablePredicate.ShowablePredicate a)	-- ^ Attempt to find the appropriate 'ShowablePredicate.ShowablePredicate' to implement this /Perl-style shortcut/.

{- |
	* A /BracketExpression/ can contain either a literal, a range of literals given @(Enum a, Ord a)@, a /Perl-style shortcut/, or when 'Char' is the type-parameter, a /POSIX Character-class/.

	* This data-type reduces the representation of all these possibilities to either a predicate or a literal.
-}
data Member m	=
	Predicate (ShowablePredicate.ShowablePredicate m)	-- ^ This 'Member' is described using a /predicate/, which is run to determine whether the datum conforms & is a member of the "BracketExpression".
	| Literal m						-- ^ This 'Member' is defined literally, using an item of the polymorphic type.
	deriving Eq

instance Show m => Show (Member m)	where
	showsPrec _ (Predicate showablePredicate)	= shows showablePredicate
	showsPrec _ (Literal literal)			= shows literal

instance (ShortcutExpander m, Read m) => Read (Member m)	where
	readsPrec _ []				= []		-- No parse.
	readsPrec _ (' ' : s)			= reads s	-- Consume white-space.
	readsPrec _ ('\t' : s)			= reads s	-- Consume white-space.
	readsPrec _ ('\\' : shortcut : s)	= case findPredicate shortcut of
		Just showablePredicate	-> [(Predicate showablePredicate, s)]
		_			-> error $ "readsPrec RegExDot.BracketExpressionMember.Member:\tfindPredicate failed for shortcut " ++ show shortcut
	readsPrec _ literal			= Control.Arrow.first Literal `map` reads literal

instance Control.DeepSeq.NFData m => Control.DeepSeq.NFData (Member m)	where
	rnf (Predicate showablePredicate)	= Control.DeepSeq.rnf showablePredicate
	rnf (Literal literal)			= Control.DeepSeq.rnf literal

-- | Match-operator.
(=~) :: Eq m
	=> m		-- ^ The input datum.
	-> Member m	-- ^ The member of the bracket-expression against which the input-datum is to be matched.
	-> Bool		-- ^ The result of the match-operation.
datum =~ Predicate showablePredicate	= ShowablePredicate.predicate showablePredicate datum
datum =~ Literal literal		= datum == literal

