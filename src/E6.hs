{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E6
  ( firstTrue
  , secondTrue
  , evenElems
  , oddElems
  , firstTuple
  , secondTuple
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

-- Exercises Missing Values
-- 1. Write an optic which focuses the value at key "first" or, failing that, the value at key "second"
optic = (ix "first") `failing` (ix "second")

firstTrue = M.fromList [("first", False), ("second", False)] & optic .~ True

secondTrue = M.fromList [("second", False)] & optic .~ True

-- 2. Write an optic which focuses the first element of a tuple iff it is even and the second
-- tuple elemnet otherwise. Assume each slot contains an integer.
optic'' = _1 . filtered even `failing` _2

secondTuple = (1, 1) & optic'' *~ 10

firstTuple = (2, 2) & optic'' *~ 10

-- 3. Write an optic which focuses all even numbers in a list, if non of the members are even then
-- focus ALL numbers in the list.
optic' = (traversed . filtered even) `failing` traversed

evenElems = [1, 2, 3, 4] ^.. optic'

oddElems = [1, 3, 5] ^.. optic'
