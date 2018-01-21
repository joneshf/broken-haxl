{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Functor.Identity
import Haxl.Core

main :: IO ()
main = do
  putStrLn "hello world"

newtype Foo a
  = Foo (GenHaxl () (Identity a))
  deriving (Functor)
