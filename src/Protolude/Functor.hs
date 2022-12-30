{-# LANGUAGE CPP #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Functor (
  Functor(fmap),
  ($>),
  (<$),
  (<$>),
  (<<$>>),
  (<&>),
  void,
) where

import Data.Function ((.), flip)
import Data.Functor (
    Functor(fmap)
  , (<$)
  , ($>)
  , (<$>)
  , (<&>)
  , void
  )

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap
