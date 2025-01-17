{-# OPTIONS_GHC -fignore-interface-pragmas #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module A where

import GHC.Generics (Generic)
import Rel8

data Foo f = Foo
  { foo00 :: Column f String,
    foo01 :: Column f String,
    foo02 :: Column f String,
    foo03 :: Column f String,
    foo04 :: Column f String,
    foo05 :: Column f String,
    foo06 :: Column f String,
    foo07 :: Column f String,
    foo08 :: Column f String,
    foo09 :: Column f String,
    foo10 :: Column f String,
    foo11 :: Column f String,
    foo12 :: Column f String,
    foo13 :: Column f String,
    foo14 :: Column f String,
    foo15 :: Column f String,
    foo16 :: Column f String,
    foo17 :: Column f String,
    foo18 :: Column f String,
    foo19 :: Column f String,
    foo20 :: Column f String,
    foo21 :: Column f String,
    foo22 :: Column f String,
    foo23 :: Column f String,
    foo24 :: Column f String,
    foo25 :: Column f String,
    foo26 :: Column f String,
    foo27 :: Column f String,
    foo28 :: Column f String,
    foo29 :: Column f String
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)
