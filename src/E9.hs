{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E9
  ( _Email
  , _Telephone
  , _Address
  , addToRight
  , filterJust
  , updateJust
  , filterJustTuple
  , consToString
  , wrapEitherJust
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

--Prism exercises
--1.Which Prisms will be generated from the following data declaration?
data ContactInfo
  = Email String
  | Telephone Int
  | Address String String String

makePrisms ''ContactInfo

-- _Email: Prism' ContactInfo String
-- _Telephone: Prism' ContactInfo String
-- _Address: Prism' ContactInfo (String, String, String)
--2.Fill in the blanks
addToRight = Right 35 & _Right +~ 5

filterJust =
  [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] ^..
  folded .
  _Just

updateJust =
  [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] &
  traversed .
  _Just <>~
  "Stone"

addToList = _Cons # ("Do", ["Re", "Mi"])

notAnInt = isn't (_Show :: Prism' String Int) "not an int"

--3. Write an expression to get the output from the provided input

-- input = (Just 1, Nothing, Just 3)
-- output = [1,3]
filterJustTuple :: (Maybe Int, Maybe Int, Maybe Int) -> [Int]
filterJustTuple = toListOf (each . _Just)

-- input = ('y', "yz")
-- output = "xyz"

consToString :: (Char, String) -> String
consToString = (#) _Cons 

-- input = "do the hokey pokey"
-- output = Left (Just (Right "do the hokey pokey"))

wrapEitherJust = (#) (_Left . _Just . _Right)