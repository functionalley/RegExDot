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

 [@DESCRIPTION@]	Permits transformation of 'RegEx.MatchList', to facilitate standardisation.
-}

module RegExDot.DataSpanTree(
-- * Types
-- ** Type-synonyms
--	DataSpanTree,
--	DataSpanTreeList,
-- * Functions
	extractCaptureGroups,
	flattenTreeList,
	toTreeList
) where

import qualified	Data.Foldable
import qualified	RegExDot.ConsumptionBounds	as ConsumptionBounds
import qualified	RegExDot.DataSpan		as DataSpan
import qualified	RegExDot.RegEx			as RegEx
import qualified	RegExDot.Tree			as Tree

-- | Defines a similar 'Tree.Tree' to 'RegEx.Match', but with different 'Tree.Leaf's.
type DataSpanTree a	= Tree.Tree (DataSpan.DataSpan a)

-- | Defines a similar structure to 'RegEx.MatchList'.
type DataSpanTreeList a	= [DataSpanTree a]

-- | Converts a 'RegEx.MatchList' into a 'DataSpanTreeList', by transforming the 'Tree.Leaf's.
toTreeList :: RegEx.MatchList a -> DataSpanTreeList a
toTreeList	= map toTree	where
	toTree :: RegEx.Match a -> DataSpanTree a
	toTree	= fmap toDataSpan	where
		toDataSpan :: RegEx.MatchedData a -> DataSpan.DataSpan a
		toDataSpan (_, inputDataOffset, inputData)	= (inputData, (inputDataOffset, length inputData))

-- | Condenses a 'DataSpanTreeList's into a list of 'DataSpan.DataSpan's, using 'DataSpan.join'.
flattenTreeList
	:: ConsumptionBounds.DataLength	-- ^ The offset into the input-data at which a match occurred.
	-> DataSpanTreeList a		-- ^ The tree to flatten.
	-> [DataSpan.DataSpan a]
flattenTreeList _ []				= []
flattenTreeList offset (tree : treeList)	= flattenedTree : flattenTreeList (DataSpan.after flattenedTree) treeList	where
--	flattenedTree :: DataSpan.DataSpan a
	flattenedTree	= DataSpan.join offset $ Data.Foldable.toList tree

{- |
	* POSIX describes the contents of /capture-groups/, as summarised in <http://www2.research.att.com/~gsf/testregex/>.

	* 'RegEx.Result', is a complete description of the match between 'RegEx.InputData' & RegEx.ExtendedRegEx'; this function extracts a POSIX-conformant list from it.

	* The major differences are, that:

		Only data from parenthesized sub-expressions ('RegEx.Alternatives') is captured.

		Only the /last/ repetition of a repeated sub-expression is returned.
		<http://www.opengroup.org/onlinepubs/009695399/functions/regcomp.html>.

		The data captured within each parenthesized sub-expression, is summarised as a single 'DataSpan.DataSpan'.

		POSIX specifies a 'Span.Span'-offset of @-1@, for sub-expressions which match zero times; cf sub-expressions which consume nothing, once.
		<http://www.opengroup.org/onlinepubs/009695399/functions/regcomp.html>.
		@
			("ace" Text.Regex.Posix.=~ "a(b)*c(d)?e") :: Text.Regex.Base.RegexLike.MatchArray
			array (0,2) [(0,(0,3)),(1,(-1,0)),(2,(-1,0))]

			("ace" Text.Regex.Posix.=~ "a(b*)c(d?)e") :: Text.Regex.Base.RegexLike.MatchArray
			array (0,2) [(0,(0,3)),(1,(1,0)),(2,(2,0))]
		@
		I consider this a poor convention, resulting from the focus of POSIX on C, which makes subsequent calculation from the list of 'DataSpan's difficult & error-prone.
-}
extractCaptureGroups
	:: Bool			-- ^ Whether to strictly comply with /POSIX/.
	-> DataSpanTreeList a	-- ^ The tree-structure from which to extract the capture-groups.
	-> [DataSpan.DataSpan a]
extractCaptureGroups complyStrictlyWithPosix	= extractCaptureGroups' 0	where
	extractCaptureGroups' :: ConsumptionBounds.DataLength -> DataSpanTreeList a -> [DataSpan.DataSpan a]
	extractCaptureGroups' _ []			= []
	extractCaptureGroups' offset (tree : treeList)	= let
--		recurseHorizontallyFrom :: ConsumptionBounds.DataLength -> [DataSpan.DataSpan a]
		recurseHorizontallyFrom	= (`extractCaptureGroups'` treeList)
	 in case tree of
		Tree.Leaf dataSpan	-> recurseHorizontallyFrom $ DataSpan.after dataSpan
		Tree.Node []		-> DataSpan.empty (if complyStrictlyWithPosix then -1 else offset) : recurseHorizontallyFrom offset	-- POSIX specifies an Span-offset of -1, for sub-expressions which match 0 times; cf sub-expressions which consumes nothing, once.
		Tree.Node treeLists	-> joinedFlattenedTreeList : (extractCaptureGroups' offset lastMatch {-recurse vertically-} ++ recurseHorizontallyFrom (DataSpan.after joinedFlattenedTreeList))	where
--			lastMatch :: DataSpanTreeList a
			lastMatch	= last treeLists	-- <http://www.opengroup.org/onlinepubs/009695399/functions/regcomp.html>.

--			joinedFlattenedTreeList :: DataSpan.DataSpan a
			joinedFlattenedTreeList	= DataSpan.join offset $ flattenTreeList offset lastMatch

