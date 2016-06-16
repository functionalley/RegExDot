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

	* Defines a specific type of tree-structure, which is composed from either a datum, or a list of Tree-lists; <https://en.wikipedia.org/wiki/Rose_Tree>.

	* This more general tree-structure has the shape of a regular-expression match.

	* This contrasts with the typical binary-tree, which is either empty, or contains a (left Tree, datum, right Tree).
-}

module RegExDot.Tree(
-- * Types
-- ** Type-synonyms
--	TreeList,
-- ** Data-types
	Tree(..),
-- * Functions
	pop
) where

import qualified	Control.Arrow
import qualified	Control.DeepSeq
import qualified	Data.Foldable
import qualified	Data.List
import qualified	Data.Monoid

-- | A general purpose tree-type structure.
data Tree a	=
	Leaf a			-- ^ The payload.
	| Node [TreeList a]	-- ^ Recurse. N.B.: a list of lists is required to contain the /MatchLists/ resulting from repeated /Alternatives/.
	deriving Eq

-- | A list of 'Tree's; significant only because it is the essence of the recursive nature of 'Tree'.
type TreeList a	= [Tree a]

instance Show a => Show (Tree a)	where
	showsPrec _ (Leaf a)		= shows a
	showsPrec _ (Node treeLists)	= showList treeLists

instance Read a => Read (Tree a)	where
	readsPrec _ node@('[' : _)	= Control.Arrow.first Node `map` readList node {-singleton-}
	readsPrec _ leaf		= Control.Arrow.first Leaf `map` reads leaf {-singleton-}

instance Control.DeepSeq.NFData a => Control.DeepSeq.NFData (Tree a)	where
	rnf (Leaf a)	= Control.DeepSeq.rnf a
	rnf (Node l)	= Control.DeepSeq.rnf l

instance Functor Tree	where
	fmap f (Leaf a)		= Leaf $ f a
	fmap f (Node treeLists)	= Node $ map (fmap {-recurse-} f `map`) treeLists

instance Data.Foldable.Foldable Tree where
	foldMap f (Leaf a)		= f a	-- CAVEAT: 'f' should be Associative, as required by a Monoid.
	foldMap f (Node treeLists)	= Data.List.foldl' (Data.List.foldl' (\monoid -> (monoid `Data.Monoid.mappend`) . Data.Foldable.foldMap f)) Data.Monoid.mempty treeLists

-- | Deconstruct the specified 'Node'; i.e. lop the apex from the 'Tree', leaving a flat top.
pop
	:: Tree a	-- ^ The tree from which to extract the list of trees hanging immediately beneath its apex.
	-> [TreeList a]
pop (Node treeLists)	= treeLists
pop _			= error "RegExDot.Tree.pop:\tunexpected Leaf"

