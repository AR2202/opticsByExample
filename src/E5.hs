{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E5
  ( extractingNumbers
  , evenTimes10
  , aardvarkToKangaroo
  , antdvark
  , shiftByOne
  , reverseTriple
  , reverse3
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

-- Exercises partsOf
extractingNumbers =
  ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^.
  partsOf (beside traversed traversed)

evenTimes10 = [1, 2, 3, 4] & partsOf (traversed . filtered even) .~ [20, 40]

aardvarkToKangaroo =
  ["Aardvark", "Bandicoot", "Capybara"] &
  partsOf (traversed . traversed) .~ "Kangaroo"

antdvark =
  ["Aardvark", "Bandicoot", "Capybara"] &
  partsOf (traversed . traversed) .~ "Ant"

shiftByOne =
  M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] &
  partsOf traversed %~ (tail . cycle)

reverseTriple = ('a', 'b', 'c') & partsOf each %~ reverse

-- Bonus
reverse3 = [1, 2, 3, 4, 5, 6] & partsOf (traversed . filtered (< 4)) %~ reverse
