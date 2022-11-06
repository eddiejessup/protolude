{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}

module Protolude.CallStack
( HasCallStack
) where

import GHC.Stack (HasCallStack)
