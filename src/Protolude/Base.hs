{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Protolude.Base (
  module Base,
  ($!),
) where

-- Glorious Glasgow Haskell Compiler

-- Base GHC types
import GHC.Num as Base (
    Num(
      (+),
      (-),
      (*),
      negate,
      abs,
      signum,
      fromInteger
    )
  , Integer
  , subtract
  )
import GHC.Enum as Base (
    Bounded(minBound, maxBound)
  , Enum(
      succ,
      pred,
      toEnum,
      fromEnum,
      enumFrom,
      enumFromThen,
      enumFromTo,
      enumFromThenTo
    )
  , boundedEnumFrom
  , boundedEnumFromThen
  )
import GHC.Real as Base (
    (%)
  , (/)
  , Fractional
  , Integral
  , Ratio
  , Rational
  , Real
  , RealFrac
  , (^)
  , (^%^)
  , (^^)
  , (^^%^^)
  , ceiling
  , denominator
  , div
  , divMod
  , divZeroError
  , even
  , floor
  , fromIntegral
  , fromRational
  , gcd
  , infinity
  , integralEnumFrom
  , integralEnumFromThen
  , integralEnumFromThenTo
  , integralEnumFromTo
  , lcm
  , mod
  , notANumber
  , numerator
  , numericEnumFrom
  , numericEnumFromThen
  , numericEnumFromThenTo
  , numericEnumFromTo
  , odd
  , overflowError
  , properFraction
  , quot
  , quotRem
  , ratioPrec
  , ratioPrec1
  , ratioZeroDenominatorError
  , realToFrac
  , recip
  , reduce
  , rem
  , round
  , showSigned
  , toInteger
  , toRational
  , truncate
  , underflowError
  )
import GHC.Float as Base (
    Float(F#)
  , Double(D#)
  , Floating (..)
  , RealFloat(..)
  , showFloat
  , showSignedFloat
  )
import GHC.Show as Base (
    Show(showsPrec, show, showList)
  )
import GHC.Exts as Base (
    Constraint
  , Ptr
  , FunPtr
  )
import GHC.Base as Base (
    (++)
  , seq
  , asTypeOf
  , ord
  , maxInt
  , minInt
  , until
  )

-- Exported for lifting into new functions.
import System.IO as Base (
    print
  , putStr
  , putStrLn
  )

import GHC.Types as Base (
    Bool
  , Char
  , Int
  , Word
  , Ordering
  , IO
  , Coercible
  )

import GHC.StaticPtr as Base (StaticPtr)

import GHC.OverloadedLabels as Base (
    IsLabel(fromLabel)
  )

import GHC.ExecutionStack as Base (
    Location(Location, srcLoc, objectName, functionName)
  , SrcLoc(SrcLoc, sourceColumn, sourceLine, sourceColumn)
  , getStackTrace
  , showStackTrace
  )

import GHC.Stack as Base (
    CallStack
  , type HasCallStack
  , callStack
  , prettySrcLoc
  , currentCallStack
  , getCallStack
  , prettyCallStack
  , withFrozenCallStack
  )

import GHC.TypeLits as Base (
    Symbol
  , SomeSymbol(SomeSymbol)
  , Nat
  , SomeNat(SomeNat)
  , CmpNat
  , KnownSymbol
  , KnownNat
  , natVal
  , someNatVal
  , symbolVal
  , someSymbolVal
  )

import GHC.Records as Base (
    HasField(getField)
  )

import Data.Kind as Base (
  type Type
  )

-- Default Prelude defines this at the toplevel module, so we do as well.
infixr 0 $!

($!) :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx
