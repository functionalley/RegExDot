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

 [@DESCRIPTION@]	Used to implement /Perl-shortcut/s, /BracketExpression/-ranges & /POSIX Character-classes/.
-}

module RegExDot.ShowablePredicate(
-- * Types
	Predicate,
-- ** Data-types
	ShowablePredicate(
		MkShowablePredicate,
		name,
		predicate
	)
) where

import qualified	Control.DeepSeq

-- | An arbitrary polymorphic predicate function.
type Predicate a	= a -> Bool

-- | Container for both the /predicate/, & the name used in the implementation of 'Show'.
data ShowablePredicate a	= MkShowablePredicate {
	name		:: String,	-- ^ The identifier of this predicate when in string-form.
	predicate	:: Predicate a	-- ^ The function used to determine whether an input datum matches.
}

instance Show (ShowablePredicate a)	where
	showsPrec _	= showString . name

instance Eq (ShowablePredicate a)	where
	l == r	= name l == name r	-- Ignore 'predicate'.

instance Control.DeepSeq.NFData (ShowablePredicate a)	where
	rnf	= Control.DeepSeq.rnf . name	-- Ignore 'predicate'.

