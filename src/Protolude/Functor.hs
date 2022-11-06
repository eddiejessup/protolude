{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Functor (
  Functor(fmap),
  ($>),
  (<$),
  (<$>),
  (<<$>>),
  (<&>),
  void,
  foreach,
) where

import Data.Function ((.), flip)
import Data.Functor ((<&>))

import Data.Functor (
    Functor(fmap)
  , (<$)
  , ($>)
  , (<$>)
  , void
  )

infixl 4 <<$>>

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

foreach :: Functor f => f a -> (a -> b) -> f b
foreach = flip fmap
