{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Unsafe (
  unsafeHead,
  unsafeTail,
  unsafeInit,
  unsafeLast,
  unsafeFromJust,
  unsafeIndex,
  unsafeThrow,
  unsafeRead,
) where

import Protolude.Base (Int)

import Protolude.Base (HasCallStack)
import Data.Char (Char)
import Text.Read (Read, read)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Control.Exception as Exc

unsafeThrow :: Exc.Exception e => e -> a
unsafeThrow = Exc.throw

unsafeHead :: HasCallStack => [a] -> a
unsafeHead = List.head

unsafeTail :: HasCallStack => [a] -> [a]
unsafeTail = List.tail

unsafeInit :: HasCallStack => [a] -> [a]
unsafeInit = List.init

unsafeLast :: HasCallStack => [a] -> a
unsafeLast = List.last

unsafeFromJust :: HasCallStack => Maybe.Maybe a -> a
unsafeFromJust = Maybe.fromJust

unsafeIndex :: HasCallStack => [a] -> Int -> a
unsafeIndex = (List.!!)

unsafeRead :: (HasCallStack, Read a) => [Char] -> a
unsafeRead = Text.Read.read
