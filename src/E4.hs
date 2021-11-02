{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E4
  ( wordedCompose
  , wordedTwice
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

-- Exercises Traversal laws
-- show that worded is not a lawful traversal
-- worded breaks the second law
-- with composition of handlers:
wordedCompose = "a string" & worded %~ (++ "world") . (++ " hello ")

-- using worded twice with separate handlers (not the same as composition of handlers)
wordedTwice = "a string" & worded %~ (++ " hello ") & worded %~ (++ "world")
