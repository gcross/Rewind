{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rewind.Level where

import Control.Applicative (Applicative(..))
import Control.Lens
    (At(..)
    ,Contains(..)
    ,Contravariant(..)
    ,Index
    ,Ixed(..)
    ,IxValue
    ,(^.)
    ,makeLenses
    )

import Data.Map (Map)
import Data.Word

data XY = XY
    { _x :: {-# UNPACK #-} !Word
    , _y :: {-# UNPACK #-} !Word
    } deriving (Eq,Ord,Read,Show)
makeLenses ''XY

data Place =
    Wall
  | Floor

data Level = Level
    {   _width :: Word
    ,   _height :: Word
    ,   _places :: Map XY Place
    }
makeLenses ''Level

type instance Index Level = XY
type instance IxValue Level = Place

instance At Level where
    at xy = places . at xy

instance Applicative f ⇒ Ixed f Level where
    ix xy = places . ix xy

instance (Contravariant k, Functor k) => Contains k Level where
    contains xy = places . contains xy