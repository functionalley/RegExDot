{-# OPTIONS_GHC -fno-warn-orphans #-}
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

 [@DESCRIPTION@]	Provides a default implementation of the type-classes, required for 'RegEx.ExtendedRegEx' Int.

 [@CAVEATS@]

	In contrast to the traditional regex, there's no reason to escape a 'Char', other than when referring to a /shortcut/.
	Since no /shortcuts/ are defined in this minimal definition, it's always an error to escape a character.
-}

module RegExDot.InstanceInt() where

import qualified	RegExDot.BracketExpressionMember	as BracketExpressionMember
import qualified	RegExDot.Meta				as Meta
import qualified	RegExDot.RegEx				as RegEx

instance BracketExpressionMember.ShortcutExpander Int	where
	findPredicate _	= Nothing

instance Meta.ShortcutExpander Int	where
	expand c	= error $ "RegExDot.Meta.ShortcutExpander.expand RegExDot.InstanceInt:\tunrecognised shortcut '" ++ show c ++ "'."	-- A shortcut is the only legitimate reason to escape a character.

instance RegEx.ShortcutExpander Int	where
	expand c	= error $ "RegExDot.RegEx.ShortcutExpander.expand RegExDot.InstanceInt:\tunrecognised shortcut '" ++ show c ++ "'."	-- A shortcut is the only legitimate reason to escape a character.

