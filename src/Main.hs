{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Foldable (for_)
import Haxl.Core

main :: IO ()
main = do
  putStrLn "hello world"

data Foo a where
    Foo :: Foo ()

deriving instance Eq (Foo a)
deriving instance Show (Foo a)

instance ShowP Foo where
   showp = show

instance StateKey Foo where
   data State Foo = Foo'

instance DataSourceName Foo where
   dataSourceName _ = "Foo"

instance DataSource a Foo where
   fetch _ _ _ blockedFetches = SyncFetch $ do
      for_ blockedFetches $ \(BlockedFetch Foo r) -> do
         pure ()
