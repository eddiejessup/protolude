{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Protolude (
  -- * Base functions
  module Base,
  identity,
  pass,
  -- * Function functions
  module Function,
  applyN,
  -- * List functions
  module List,
  map,
  uncons,
  unsnoc,
  -- * Data Structures
  module DataStructures,
  -- * Show functions
  module Show,
  show,
  -- * Bool functions
  module Bool,
  -- * Monad functions
  lift,
  module Monad,
  -- * Functor functions
  module Functor,
  -- * Either functions
  module Either,
  -- * Applicative functions
  module Applicative,
  guarded,
  guardedA,
  -- * String conversion
  module ConvertText,
  -- * Debug functions
  module Debug,

  -- * Panic functions
  module Panic,
  -- * Exception functions
  module Exception,

  -- * String functions
  module String,
  -- * Safe functions
  module Safe,
  -- * Eq functions
  module Eq,
  -- * Ord functions
  module Ord,
  -- * Traversable functions
  module Traversable,
  -- * Foldable functions
  module Foldable,
  -- * Semigroup functions
  module Semigroup,
  -- * Monoid functions
  module Monoid,
  -- * Bifunctor functions
  module Bifunctor,
  -- * Bifunctor functions
  module Hashable,

  -- * Deepseq functions
  module DeepSeq,

  -- * Tuple functions
  module Tuple,

  module Typeable,

  -- * Typelevel programming
  module Typelevel,

  -- * Integers
  module Int,
  module Bits,

  -- * Complex functions
  module Complex,

  -- * Char functions
  module Char,

  -- * Maybe functions
  module Maybe,

  -- * Generics functions
  module Generics,

  -- * ByteString functions
  module ByteString,
  LByteString,

  -- * Text functions
  module Text,
  LText,

  -- * Read functions
  module Read,
  readMaybe,
  readEither,

  -- * System functions
  module System,
) where

-- Protolude module exports.
import Protolude.Debug as Debug
import Protolude.List as List
import Protolude.Show as Show
import Protolude.Bool as Bool
import Protolude.Monad as Monad
import Protolude.Functor as Functor
import Protolude.Either as Either
import Protolude.ConvertText as ConvertText
import Protolude.Panic as Panic
import Protolude.Exceptions as Exception
import qualified Protolude.Conv as Conv

import Protolude.Base as Base hiding (
    putStr           -- Overriden by Show.putStr
  , putStrLn         -- Overriden by Show.putStrLn
  , print            -- Overriden by Protolude.print
  , show             -- Overriden by Protolude.show
  , showFloat        -- Custom Show instances deprecated.
  , showList         -- Custom Show instances deprecated.
  , showSigned       -- Custom Show instances deprecated.
  , showSignedFloat  -- Custom Show instances deprecated.
  , showsPrec        -- Custom Show instances deprecated.
  )
import qualified Protolude.Base as PBase

-- Used for 'show', not exported.
import Data.String (String)
import Data.String as String (IsString)

-- Maybe'ized version of partial functions
import Protolude.Safe as Safe (
    headMay
  , headDef
  , initMay
  , initDef
  , initSafe
  , tailMay
  , tailDef
  , tailSafe
  , lastDef
  , lastMay
  , foldr1May
  , foldl1May
  , foldl1May'
  , maximumMay
  , minimumMay
  , maximumDef
  , minimumDef
  , atMay
  , atDef
  )

-- Applicatives
import Control.Applicative as Applicative (
    Applicative(..)
  , Alternative(..)
  , Const(Const,getConst)
  , ZipList(ZipList,getZipList)
  , (<**>)
  , liftA
  , liftA2
  , liftA3
  , optional
  )

-- Base typeclasses
import Data.Eq as Eq (
    Eq(..)
  )
import Data.Ord as Ord (
    Ord(..)
  , Ordering(LT,EQ,GT)
  , Down(Down)
  , comparing
  )
import Data.Traversable as Traversable
import Data.Foldable as Foldable (
    Foldable,
    fold,
    foldMap,
    foldr,
    foldr',
    foldl,
    foldl',
    toList,
    null,
    length,
    elem,
    maximum,
    minimum,
    foldrM,
    foldlM,
    traverse_,
    for_,
    mapM_,
    forM_,
    sequence_,
    sequenceA_,
    asum,
    msum,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find,
  )
import Data.Functor.Identity as Functor (
    Identity(Identity, runIdentity)
  )

import Data.List.NonEmpty as List (
    NonEmpty((:|))
  , nonEmpty
  )
import Data.Semigroup as Semigroup (
    Semigroup(sconcat, stimes)
  , WrappedMonoid
  , diff
  , cycle1
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
  )

import Data.Monoid as Monoid

import Data.Bifunctor as Bifunctor (Bifunctor(bimap, first, second))

-- Deepseq
import Control.DeepSeq as DeepSeq (
    NFData(..)
  , ($!!)
  , deepseq
  , force
  )

-- Data structures
import Data.Tuple as Tuple (
    fst
  , snd
  , curry
  , uncurry
  , swap
  )

import Data.List as List (
    splitAt
  , break
  , intercalate
  , isPrefixOf
  , isInfixOf
  , isSuffixOf
  , drop
  , filter
  , reverse
  , replicate
  , take
  , sortBy
  , sort
  , intersperse
  , transpose
  , subsequences
  , permutations
  , scanl
  , scanl'
  , scanr
  , iterate
  , repeat
  , cycle
  , unfoldr
  , takeWhile
  , dropWhile
  , group
  , inits
  , tails
  , zipWith
  , zip
  , unzip
  , genericLength
  , genericTake
  , genericDrop
  , genericSplitAt
  , genericReplicate
  )

-- Hashing
import Data.Hashable as Hashable (
    Hashable
  , hash
  , hashWithSalt
  , hashUsing
  )

import Data.Map as DataStructures (Map)
import Data.Set as DataStructures (Set)
import Data.Sequence as DataStructures (Seq)
import Data.IntMap as DataStructures (IntMap)
import Data.IntSet as DataStructures (IntSet)

import Data.Typeable as Typeable (
    TypeRep
  , Typeable
  , typeOf
  , cast
  , gcast
  , typeRep
  , eqT
  )

import Data.Proxy as Typelevel (
    Proxy(..)
  )

import Data.Type.Coercion as Typelevel (
    Coercion(..)
  , coerceWith
  , repr
  )

import Data.Type.Equality as Typelevel (
    (:~:)(..)
  , type (==)
  , sym
  , trans
  , castWith
  , gcastWith
  )

import Data.Void as Typelevel (
    Void
  , absurd
  , vacuous
  )

import Control.Monad.Trans as Trans (
    lift
  )

-- Base types
import Data.Int as Int (
    Int
  , Int8
  , Int16
  , Int32
  , Int64
  )
import Data.Bits as Bits (
  Bits,
  (.&.),
  (.|.),
  xor,
  complement,
  shift,
  rotate,
  zeroBits,
  bit,
  setBit,
  clearBit,
  complementBit,
  testBit,
  bitSizeMaybe,
  bitSize,
  isSigned,
  shiftL,
  shiftR,
  rotateL,
  rotateR,
  popCount,
  FiniteBits,
  finiteBitSize,
  bitDefault,
  testBitDefault,
  popCountDefault,
  toIntegralSized,
  countLeadingZeros,
  countTrailingZeros,
  )
import Data.Word as Bits (
    Word
  , Word8
  , Word16
  , Word32
  , Word64
  , byteSwap16
  , byteSwap32
  , byteSwap64
  )

import Data.Either as Either (
    Either(Left,Right)
  , either
  , lefts
  , rights
  , partitionEithers
  , isLeft
  , isRight
  )

import Data.Complex as Complex (
    Complex((:+))
  , realPart
  , imagPart
  , mkPolar
  , cis
  , polar
  , magnitude
  , phase
  , conjugate
  )
import Data.Char as Char (
    Char
  , ord
  , chr
  , digitToInt
  , intToDigit
  , toUpper
  , toLower
  , toTitle
  , isAscii
  , isLetter
  , isDigit
  , isHexDigit
  , isPrint
  , isAlpha
  , isAlphaNum
  , isUpper
  , isLower
  , isSpace
  , isControl
  )
import Data.Bool as Bool (
  Bool(True, False),
  (&&),
  (||),
  not,
  otherwise
  )
import Data.Maybe as Maybe (
    Maybe(Nothing, Just)
  , maybe
  , isJust
  , isNothing
  , fromMaybe
  , listToMaybe
  , maybeToList
  , catMaybes
  , mapMaybe
  )

import Data.Function as Function (
    const
  , (.)
  , ($)
  , flip
  , fix
  , on
  , (&)
  )

-- Genericss
import GHC.Generics as Generics (
    Generic(..)
  , Generic1
  , Rep
  , K1(..)
  , M1(..)
  , U1(..)
  , V1
  , D1
  , C1
  , S1
  , (:+:)(..)
  , (:*:)(..)
  , (:.:)(..)
  , Rec0
  , Constructor(..)
  , Datatype(..)
  , Selector(..)
  , Fixity(..)
  , Associativity(..)
  , Meta(..)
  , FixityI(..)
  , URec
  )

-- ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString as ByteString (ByteString)

-- Text
import Data.Text as Text (
    Text
  , lines
  , words
  , unlines
  , unwords
  )
import qualified Data.Text.Lazy

import Data.Text.IO as Text (
    getLine
  , getContents
  , interact
  , readFile
  , writeFile
  , appendFile
  )

import Data.Text.Lazy as Text (
    toStrict
  , fromStrict
  )

import Data.Text.Encoding as Text (
    encodeUtf8
  , decodeUtf8
  , decodeUtf8'
  , decodeUtf8With
  )

import Data.Text.Encoding.Error as Text (
    OnDecodeError
  , OnError
  , UnicodeException
  , lenientDecode
  , strictDecode
  , ignore
  , replace
  )

-- IO
import System.Environment as System (getArgs)
import qualified System.Exit
import System.Exit as System (
    ExitCode(..)
  , exitWith
  , exitFailure
  , exitSuccess
  )
import System.IO as System (
    Handle
  , FilePath
  , IOMode(..)
  , stdin
  , stdout
  , stderr
  , withFile
  , openFile
  )

-- Concurrency and Parallelism
import Control.Exception as Exception (
    Exception,
    toException,
    fromException,
    displayException,
    SomeException(SomeException)
  , IOException
  , ArithException(
    Overflow,
    Underflow,
    LossOfPrecision,
    DivideByZero,
    Denormal,
    RatioZeroDenominator
    )
  , ArrayException(IndexOutOfBounds, UndefinedElement)
  , AssertionFailed(AssertionFailed)
  , SomeAsyncException(SomeAsyncException)
  , asyncExceptionToException
  , asyncExceptionFromException
  , AsyncException(StackOverflow, HeapOverflow, ThreadKilled, UserInterrupt)
  , NonTermination(NonTermination)
  , NestedAtomically(NestedAtomically)
  , BlockedIndefinitelyOnMVar(BlockedIndefinitelyOnMVar)
  , BlockedIndefinitelyOnSTM(BlockedIndefinitelyOnSTM)
  , AllocationLimitExceeded(AllocationLimitExceeded)
  , CompactionFailed(CompactionFailed)
  , Deadlock(Deadlock)
  , NoMethodError(NoMethodError)
  , PatternMatchFail(PatternMatchFail)
  , RecConError(RecConError)
  , RecSelError(RecSelError)
  , RecUpdError(RecUpdError)
  , ErrorCall(ErrorCall, ErrorCallWithLocation)
  , TypeError(TypeError)
  , ioError
  , catch
  , catches
  , Handler(Handler)
  , catchJust
  , handle
  , handleJust
  , try
  , tryJust
  , evaluate
  , mapException
  , mask
  , mask_
  , uninterruptibleMask
  , uninterruptibleMask_
  , MaskingState(..)
  , getMaskingState
  , interruptible
  , allowInterrupt
  , bracket
  , bracket_
  , bracketOnError
  , finally
  , onException
  )
import qualified Control.Exception as PException

import Control.Concurrent.MVar as Concurrency (
    MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , isEmptyMVar
  , withMVar
  , withMVarMasked
  , modifyMVar_
  , modifyMVar
  , modifyMVarMasked_
  , modifyMVarMasked
  , tryReadMVar
  , mkWeakMVar
  , addMVarFinalizer
  )
import Control.Concurrent.Chan as Concurrency (
    Chan
  , newChan
  , writeChan
  , readChan
  , dupChan
  , getChanContents
  , writeList2Chan
  )
import Control.Concurrent.QSem as Concurrency (
    QSem
  , newQSem
  , waitQSem
  , signalQSem
  )
import Control.Concurrent.QSemN as Concurrency (
    QSemN
  , newQSemN
  , waitQSemN
  , signalQSemN
  )
import Control.Concurrent as Concurrency (
    ThreadId
  , forkIO
  , forkFinally
  , forkIOWithUnmask
  , killThread
  , forkOn
  , forkOnWithUnmask
  , getNumCapabilities
  , setNumCapabilities
  , threadCapability
  , yield
  , threadDelay
  , threadWaitRead
  , threadWaitWrite
  , threadWaitReadSTM
  , threadWaitWriteSTM
  , rtsSupportsBoundThreads
  , forkOS
  , forkOSWithUnmask
  , isCurrentThreadBound
  , runInBoundThread
  , runInUnboundThread
  , mkWeakThreadId
  , myThreadId
  )
import Control.Concurrent.Async as Concurrency (
    Async(..)
  , Concurrently(..)
  , async
  , asyncBound
  , asyncOn
  , withAsync
  , withAsyncBound
  , withAsyncOn
  , wait
  , poll
  , waitCatch
  , cancel
  , cancelWith
  , asyncThreadId
  , waitAny
  , waitAnyCatch
  , waitAnyCancel
  , waitAnyCatchCancel
  , waitEither
  , waitEitherCatch
  , waitEitherCancel
  , waitEitherCatchCancel
  , waitEither_
  , waitBoth
  , link
  , link2
  , race
  , race_
  , concurrently
  )

import Foreign.Ptr as Foreign (IntPtr, WordPtr)
import Foreign.Storable as Foreign (Storable)
import Foreign.StablePtr as Foreign (StablePtr)

-- Read instances hiding unsafe builtins (read)
import qualified Text.Read as Read
import Text.Read as Read (
    Read
  , reads
  )

-- Type synonymss for lazy texts
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString


-- | The identity function, returns the give value unchanged.
identity :: a -> a
identity x = x

map :: Functor.Functor f => (a -> b) -> f a -> f b
map = Functor.fmap

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

unsnoc :: [x] -> Maybe ([x],x)
unsnoc = Foldable.foldr go Nothing
  where
    go x mxs = Just (case mxs of
       Nothing -> ([], x)
       Just (xs, e) -> (x:xs, e))

-- | Apply a function n times to a given value
applyN :: Int -> (a -> a) -> a -> a
applyN n f = Foldable.foldr (.) identity (List.replicate n f)

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
--
-- >>> readMaybe ("123" :: Text) :: Maybe Int
-- Just 123
--
-- >>> readMaybe ("hello" :: Text) :: Maybe Int
-- Nothing
readMaybe :: (Read b, Conv.StringConv a String) => a -> Maybe b
readMaybe = Read.readMaybe . Conv.toS

-- | Parse a string using the 'Read' instance.
-- Succeeds if there is exactly one valid result.
-- A 'Left' value indicates a parse error.
--
-- >>> readEither "123" :: Either Text Int
-- Right 123
--
-- >>> readEither "hello" :: Either Text Int
-- Left "Prelude.read: no parse"
readEither :: (Read a, Conv.StringConv String e, Conv.StringConv e String) => e -> Either e a
readEither = first Conv.toS . Read.readEither . Conv.toS

-- | Do nothing returning unit inside applicative.
pass :: Applicative f => f ()
pass = pure ()

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = Bool.bool empty (pure x) (p x)

guardedA :: (Functor.Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA p x = Bool.bool empty (pure x) `Functor.fmap` p x

show :: (Show a, Conv.StringConv String b) => a -> b
show x = Conv.toS (PBase.show x)
{-# SPECIALIZE show :: Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show  a => a -> String  #-}

-- | Terminate main process with failure
die :: Text -> IO a
die err = System.Exit.die (ConvertText.toS err)
