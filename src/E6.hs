{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E6
  ( replaceCurly
  , addSpiderman
  , deleteSuperman
  , itoy
  , noGum
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

-- Exercises Indexable Structures
-- 1. Fill in the blanks
replaceCurly = ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly"

herosAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]

addSpiderman = herosAndVillains & at "Spiderman" .~ Just "Goblin"

deleteSuperman = sans "Superman" herosAndVillains

itoy =
  S.fromList ['a', 'e', 'i', 'o', 'u'] & at 'y' .~ Just () & at 'i' .~ Nothing

-- 2. Use ix and at to go from the input to the output:
input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]

-- output = M.fromList [("candy bars", 13), ("ic cream", 5), ("soda", 37)]
noGum = input & sans "gum" & ix "soda" +~ 3
