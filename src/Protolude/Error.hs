{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Protolude.Error
( error
) where

import Data.Text (Text, unpack)

-- Full stack trace.

import GHC.Prim (TYPE, raise#)
import GHC.Types (RuntimeRep)
import Protolude.CallStack (HasCallStack)
import GHC.Exception (errorCallWithCallStackException)

{-# WARNING error "'error' remains in code" #-}
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => Text -> a
error s = raise# (errorCallWithCallStackException (unpack s) ?callStack)
