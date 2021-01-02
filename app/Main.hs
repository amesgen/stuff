{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Fix
import Data.String (IsString)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Dhall
import qualified Dhall.Core as Dhall
import GHC.Generics (Generic)
import Numeric.Natural

-- FromDhall (Fix f)

instance
  forall f.
  ( Functor f,
    forall a. Dhall.ToDhall a => Dhall.ToDhall (f a)
  ) =>
  Dhall.ToDhall (Fix f)
  where
  injectWith inputNormalizer = Dhall.Encoder {..}
    where
      -- TODO share: Dhall.Pi "_" declared' result
      embed fixf =
        Dhall.Lam (Dhall.makeFunctionBinding result (Dhall.Const Dhall.Type)) $
          Dhall.Lam (Dhall.makeFunctionBinding make (Dhall.Pi "_" declared' result)) $
            embed' . fixToResult $ fixf

      declared =
        Dhall.Pi result (Dhall.Const Dhall.Type) $
          Dhall.Pi "_" (Dhall.Pi "_" declared' result) result

      Dhall.Encoder embed' _ = Dhall.injectWith @(Dhall.Result f) inputNormalizer
      Dhall.Encoder _ declared' = Dhall.injectWith @(f (Dhall.Result f)) inputNormalizer

instance (Dhall.ToDhall (f (Dhall.Result f))) => Dhall.ToDhall (Dhall.Result f) where
  injectWith inputNormalizer = Dhall.Encoder {..}
    where
      embed = Dhall.App make . Dhall.embed (Dhall.injectWith inputNormalizer) . Dhall._unResult
      declared = result -- TODO ?

fixToResult :: Functor f => Fix f -> Dhall.Result f
fixToResult (Fix f) = Dhall.Result $ fixToResult <$> f

result :: IsString s => s
result = "result"

make :: IsString s => s
make = "make"

-- Example

data Expr
  = Lit Natural
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

data ExprF a
  = LitF Natural
  | AddF a a
  | MulF a a
  deriving stock (Functor, Generic)
  deriving anyclass (Dhall.FromDhall, Dhall.ToDhall)

toExpr :: Fix ExprF -> Expr
toExpr = foldFix \case
  LitF n -> Lit n
  AddF x y -> Add x y
  MulF x y -> Mul x y

fromExpr :: Expr -> Fix ExprF
fromExpr = unfoldFix \case
  Lit n -> LitF n
  Add x y -> AddF x y
  Mul x y -> MulF x y

example :: Text
example =
  [i|let e = ./app/expr.dhall
      in e.Add (e.Mul (e.Lit 3) (e.Lit 7)) (e.Add (e.Lit 1) (e.Lit 3)) |]

main :: IO ()
main = do
  T.putStrLn . Dhall.pretty . Dhall.declared $ Dhall.inject @(ExprF (Dhall.Result ExprF))
  traverse T.putStrLn $ Dhall.pretty <$> Dhall.expected (Dhall.auto @(ExprF (Dhall.Result ExprF)))
  expr <- toExpr <$> Dhall.input Dhall.auto example
  print expr

  let dexpr = Dhall.embed Dhall.inject $ fromExpr expr
      dexprp = Dhall.pretty dexpr
  expr <- toExpr <$> Dhall.input Dhall.auto dexprp
  print expr
