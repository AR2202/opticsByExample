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
  , amountT
  , amount
  , transactionDelta
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

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

amountT :: Traversal' Transaction Int
amountT f (Withdrawal n) = Withdrawal <$> f n
amountT f (Deposit n)    = Deposit <$> f n

transactionDelta :: Traversal' Transaction Int
transactionDelta f (Withdrawal n) = Withdrawal . negate <$> f (negate n)
transactionDelta f (Deposit n)    = Deposit <$> f n
