{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}

module Lib
  where

import Data.Semigroup

data User
  = LoggedIn { id :: Int, name :: String }
  | Anonymous
  deriving (Show, Eq)

user = LoggedIn 123 "Chris"

data Foo a
  = Foo a String
  deriving (Functor, Show)

foo = Foo 42 "hello"
foo' = (+ 1) <$> foo

newtype Price = Price Int
  deriving Semigroup via Sum Int
  deriving Show

someFunc :: IO ()
someFunc = do
  putStrLn $ show user
  putStrLn $ show $ foo'
  putStrLn $ show $ (Price 10) <> (Price 20)
