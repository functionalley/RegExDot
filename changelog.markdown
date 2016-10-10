# 2010-11-18 Dr. Alistair Ward <regexdot@functionalley.eu>

## 0.9.0.0
* First version of the package.

## 0.10.0.0
* Reacted to major-number changes in the package "**ToolShed-0.10.0.0**".
* Created sub-directory "**src/RegExDot/**" & then modified module-names accordingly.
* Pacified **hlint**.
* Added a makefile.
* Removed awkward unary operators from **Repeatable.hs**.
* Qualified identifiers used in error-messages.

## 0.10.0.1
* Ported to **ghc-7.0.1**:
	+ Used definitions of `NFData` & `rnf` from new package "**deepseq**", rather than "**parallel**".
	+ Replaced reference to the deprecated function `Control.Parallel.Strategies.parFlatMap` with `concat . Control.Parallel.Strategies.parMap`.

## 0.10.1.0
* Set @threaded@ flag to @True@ by default.
* Replaced use of @threaded@-flag in source-code, with CABAL CPP-macro.
* Relocated module "**RegExDot.Options**" to "**ToolShed.Options**".

## 0.10.2.0
* Renamed package "**RegExDot**" to "**regexdot**", for compatibility with Debian's *.deb*-format.

## 0.10.2.1
* Added manually controlled flag "**llvm**" to the *.cabal*-file.
* Changed identifier for type-parameters, to better reflect its role.
* Reacted to the creation of module "**ToolShed.Defaultable**".
* Uploaded to [Hackage](http://hackage.haskell.org/package/regexdot).

## 0.10.2.2
* Amended the *.cabal*-file to more correctly specify dependency on package "**toolshed**".
* Used new module "**ToolShed.Pair**" from package "**toolshed-0.12.0.0**".
* Guarded flag "**eager-blackholing**" in the *.cabal*-file.

## 0.11.0.0
* Replaced `(+ 1)` and `(- 1)` with the faster calls `succ` and `pred`, in module "**RegExDot.RegEx**".
* Reacted to; new module-hierarchy, creation of new module "**ToolShed.Data.List.Splits**", and addition of method `ToolShed.SelfValidate.getErrors` in package "**toolshed-0.13.0.0**", and used it to improved error-reporting in `instance Read RegExDot.RegEx.ExtendedRegEx`.
* Minor reworking of `RegExDot.Repeatable.repeatableParser`.
* Replaced `System` with `System.Environment` and `System.Exit`.

## 0.11.0.1
* Added class `Eq` to the context of `RegExDot.RegEx.safeReciprocal`, for migration to **ghc-7.4**.

## 0.11.1.0
* Removed comments referring to deleted module "**ToolShed.Unsafe**", from **RegExDot.RegEx**.
* Removed `Show` from the context of functions in module "**RegExDot.RegEx**".
* Exported a new constant `RegExDot.Anchor.unanchored`.
* Refactored `RegExDot.ConsumptionProfile.withinConsumptionBounds`.

## 0.11.1.1
* Tested with **haskell-platform-2013.2.0.0**.
* Replaced preprocessor-directives with **build-depends** constraints in the *.cabal*-file.
* In function `RegExDot.RegEx.findMatch.findMatchSlave.matchPairList`, changed `fromIntegral` (which required a type-signature) to `toRational`.
* Either replaced instances of `(<$>)` with `fmap` to avoid ambiguity between modules "**Control.Applicative**" & "**Prelude**" which (from package "**base-4.8**") also exports this symbol, or hid the symbol when importing the module "**Prelude**".

## 0.11.1.2
* Added "**Default-language**"-specification to the *.cabal*-file.
* Added file "**README.markdown**".
* Converted this file to markdown-format.
* Renamed directory "**src/**" to "**src-lib/**" for consistency with other packages. 
* Used **CPP** to control the import of symbols from **Control.Applicative**.

## 0.12.0.0
* Corrected the markdown-syntax in this file.
* Uploaded to [GitHub](https://github.com/functionalley/RegExDot.git).
* Added file **.travis.yml** to control testing by <https://docs.travis-ci.com>.
* Added file **.ghci**.
* Replaced use of module **ToolShed.Defaultable** with **Data.Default**.
* Reimplemented **RegExDot.RegEx.deconstruct** using record-syntax.
* Tested with **ghc-8.0.1**.
## 0.12.0.1
* Checked that pre-processor macros are defined.
## 0.12.1.0
