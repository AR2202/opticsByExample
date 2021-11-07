{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module E1
  ( conditional
  , viewConditional1
  , setConditional1
  , conditionalUnlawful
  , viewUnlawful1
  , viewUnlawful2
  , setUnlawful1
  , setUnlawful2
  , check1stlaw
  , check2ndlaw
  , check3rdlaw
  , builderlens
  , check1stbuilder
  , check1stbuilder'
  , check2ndbuilder
  , check2ndbuilder'
  , check3rdbuilder
  , check3rdbuilder'
  , username
  , makeUser
  , fullname
  , lemonPrice
  , limePrice
  , makeProduce
  , goalA
  , goalA'
  , goalB
  , goalB'
  , convertQuotes
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Char
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

-- Exercises Lenses
conditional :: Lens' (Bool, a, a) a
conditional = lens conGetter conSetter
  where
    conGetter (True, x, y)  = x
    conGetter (False, x, y) = y
    conSetter (True, x, y) z  = (True, z, y)
    conSetter (False, x, y) z = (False, x, z)

viewConditional1 = view conditional (True, 1, 2)

setConditional1 = set conditional "you" (False, "Hello", "world")

-- | A lens that breaks all 3 laws
conditionalUnlawful :: Lens' (Bool, a, a) a
conditionalUnlawful = lens unlawfulGetter unlawfulSetter
  where
    unlawfulGetter (True, x, y)  = x
    unlawfulGetter (False, x, y) = y
    unlawfulSetter (False, x, y) z = (True, x, z)
    unlawfulSetter (True, x, y) z  = (False, z, y)

viewUnlawful1 =
  view conditionalUnlawful (set conditionalUnlawful 3 (True, 1, 2))

viewUnlawful2 =
  view conditionalUnlawful (set conditionalUnlawful 3 (False, 1, 2))

setUnlawful1 =
  set conditionalUnlawful 4 (set conditionalUnlawful 3 (True, 1, 2))

setUnlawful2 =
  set conditionalUnlawful (view conditionalUnlawful (True, 1, 2)) (True, 1, 2)

-- | check all laws
check1stlaw = viewUnlawful1 == 3

check2ndlaw = setUnlawful2 == (True, 1, 2)

check3rdlaw = setUnlawful1 == set conditionalUnlawful 4 (True, 1, 2)

data Builder =
  Builder
    { _context :: [String]
    , _build   :: [String] -> String
    }

builderlens :: Lens' Builder String
builderlens = lens getter setter
  where
    getter builder = _build builder $ _context builder
    setter builder newstring = builder {_build = const newstring}

builder1 = Builder {_context = [], _build = const "oldstring"}

builder2 = Builder {_context = ["oldstring"], _build = head}

check1stbuilder =
  view builderlens (set builderlens "newstring" builder1) == "newstring"

check1stbuilder' =
  view builderlens (set builderlens "newstring" builder2) == "newstring"

check2ndbuilder =
  view builderlens (set builderlens (view builderlens builder1) builder1) ==
  "oldstring"

check2ndbuilder' =
  view builderlens (set builderlens (view builderlens builder2) builder2) ==
  "oldstring"

check3rdbuilder =
  view
    builderlens
    (set builderlens "neweststring" (set builderlens "newstring" builder1)) ==
  "neweststring"

check3rdbuilder' =
  view
    builderlens
    (set builderlens "neweststring" (set builderlens "newstring" builder2)) ==
  "neweststring"

-- | virtual fields exercise
data User =
  User
    { _firstName :: String
    , _lastName  :: String
    , _email     :: String
    }
  deriving (Show)

makeLenses ''User

makeUser = User

username :: Lens' User String
username = lens getter setter
  where
    getter = view email
    setter user name = set email name user

fullname :: Lens' User String
fullname = lens getter setter
  where
    getter user = (view firstName user) ++ " " ++ (view lastName user)
    setter user newname =
      set
        firstName
        (takeWhile (/= ' ') newname)
        (set lastName (drop 1 $ dropWhile (/= ' ') newname) user)

-- | Self-correcting lenses
data ProducePrices =
  ProducePrices
    { _limePrice  :: Float
    , _lemonPrice :: Float
    }
  deriving (Show)

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
  where
    getter (ProducePrices lime lemon) = lime
    setter (ProducePrices lime lemon) newlime =
      ProducePrices
        roundedlime
        (min (roundedlime + 0.5) (max lemon (roundedlime - 0.5)))
      where
        roundedlime = max 0 newlime

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
  where
    getter (ProducePrices lime lemon) = lemon
    setter (ProducePrices lime lemon) newlemon =
      ProducePrices
        (min (roundedlemon + 0.5) (max lime (roundedlemon - 0.5)))
        roundedlemon
      where
        roundedlemon = max 0 newlemon

makeProduce = ProducePrices

-- | Polymorphic lenses
data Predicate a =
  Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where
    getter (Predicate p) = p
    setter _ = Predicate

-- | Operators
data Gate =
  Gate
    { _open    :: Bool
    , _oilTemp :: Float
    }
  deriving (Show)

makeLenses ''Gate

data Army =
  Army
    { _archers :: Int
    , _knights :: Int
    }
  deriving (Show)

makeLenses ''Army

data Kingdom =
  Kingdom
    { _name :: String
    , _army :: Army
    , _gate :: Gate
    }
  deriving (Show)

makeLenses ''Kingdom

duloc :: Kingdom
duloc =
  Kingdom
    { _name = "Duloc"
    , _army = Army {_archers = 22, _knights = 14}
    , _gate = Gate {_open = True, _oilTemp = 10.0}
    }

goalA =
  duloc & name %~ mappend ": a perfect place" & army . knights .~ 42 & gate .
  open .~
  False

goalA' =
  duloc & name <>~ ": a perfect place" & army . knights +~ 28 & gate . open &&~
  False

goalB =
  duloc & name .~ "Dulocinstein" & army . archers .~ 17 & army . knights .~ 26 &
  gate .
  oilTemp .~
  100

goalB' =
  duloc & name <>~ "instein" & army . archers -~ 5 & army . knights +~ 12 & gate .
  oilTemp *~
  10

opossums = (False, "opossums") & _1 ||~ True

dudley =
  ((True, "Dudley"), 55.0) & _1 . _2 <>~ " - the worst" & _2 -~ 15 & _2 //~ 2 &
  _1 .
  _2 %~
  map toUpper &
  _1 .
  _1 .~
  False

-- | Simple Folds
quotes :: [(T.Text, T.Text, T.Text)]
quotes =
  [ (T.pack "Why", T.pack "So", T.pack "Serious?")
  , (T.pack "This", T.pack "is", T.pack "SPARTA")
  ]

convertQuotes = quotes ^.. each . each . each

-- | Custom Folds
-- 1. Fill in the blanks
yerAWizard = ["Yer", "a", "wizard", "Harry"] ^.. folded . folded

twoEach = [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)

reverseEach = ["bob", "otto", "hannah"] ^.. folded . to reverse

revTuple = ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded

-- 2. Fill in the blanks
eachtimes100 = [1 .. 5] ^.. folded . to (* 100)

-- 3. BOnNUS
fivefourthreetwoone =
  [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . to show . folding reverse

secondIfFirstEven =
  [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . filtered (even . fst) .
  _2
