{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E3
  ( BankAccount(..)
  , Transaction(..)
  , User(..)
  , Account(..)
  , amountT
  , amount
  , transactionDelta
  , validateAge
  , both'
  , left''
  , beside'
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Bitraversable
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

-- Exercises Traversal actions
data User =
  User
    { _name :: String
    , _age  :: Int
    }
  deriving (Show)

makeLenses ''User

data Account =
  Account
    { _id   :: String
    , _user :: User
    }
  deriving (Show)

makeLenses ''Account

validateAge :: Account -> Either String Account
validateAge = traverseOf (user . age) validateAge'

validateAge' age
  | age < 150 && age > 0 = Right age
  | otherwise = Left "Age must be between 0 and 150"

-- Exercises Custom Traversals
data Transaction
  = Withdrawal
      { _amount :: Int
      }
  | Deposit
      { _amount :: Int
      }
  deriving (Show)

makeLenses ''Transaction

newtype BankAccount =
  BankAccount
    { _transactions :: [Transaction]
    }
  deriving (Show)

makeLenses ''BankAccount

--1. Rewrite the amount transaction lens manually as teh following traversal:
--amountT :: Traversal' Transaction Int
amountT :: Traversal' Transaction Int
amountT f (Withdrawal n) = Withdrawal <$> f n
amountT f (Deposit n)    = Deposit <$> f n

--2. Reimplement the both traversal over tuples
--both :: Traversal (a, a) (b, b) a b
both' :: Traversal (a, a) (b, b) a b
both' f (x, y) = bitraverse f f (x, y)

--3. Write the follwing custom traversal
transactionDelta :: Traversal' Transaction Int
transactionDelta f (Withdrawal n) = Withdrawal . negate <$> f (negate n)
transactionDelta f (Deposit n)    = Deposit <$> f n

--4. Implement left :: Traversal (Either a b) (Either a' b) a a'
left'' :: Traversal (Either a b) (Either a' b) a a'
left'' f (Left x)  = Left <$> f x
left'' _ (Right y) = pure (Right y)

--5. BONUS: Reimplement the beside traversal:
beside' ::
     Traversal s t a b -> Traversal s' t' a b -> Traversal (s, s') (t, t') a b
beside' f g handler (a, b) =
  (,) <$> traverseOf f handler a <*> traverseOf g handler b
