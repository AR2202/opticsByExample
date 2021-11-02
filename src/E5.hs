{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E5
  ( Stringmap(..)
  , at
  , ix
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

-- Exercises Custom indexed structures
-- implement both Ixed At for a newtype wrapper around Map
-- which makes indexing case insensitive
-- newtype wrapper
newtype Stringmap a =
  Stringmap (M.Map String a)

-- Type Family instances
type instance Index (Stringmap a) = String

type instance IxValue (Stringmap a) = a

-- Typeclass instances
instance Ixed (Stringmap a) where
  ix :: Applicative f => String -> (a -> f a) -> Stringmap a -> f (Stringmap a)
  ix i handler (Stringmap m) =
    Stringmap <$> traverseOf (ix (map toLower i)) handler m

instance At (Stringmap a) where
  at ::
       Functor f
    => String
    -> (Maybe a -> f (Maybe a))
    -> Stringmap a
    -> f (Stringmap a)
  at i handler (Stringmap m) =
    Stringmap <$> traverseOf (at (map toLower i)) handler m
