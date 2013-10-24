{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rewind.Area where

import Control.Lens
    (At(..)
    ,Contains(..)
    ,Contravariant
    ,Index
    ,Ixed(..)
    ,IxValue
    ,Lens'
    ,(&)
    ,(<&>)
    ,(^.)
    ,(%~)
    ,(.~)
    ,containsTest
    ,indexed
    ,makeLenses
    ,to
    )

import Data.Derive.Monoid
import Data.DeriveTH
import Data.Functor (Functor(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..),(<>))
import Data.Set (Set)
import qualified Data.Set as Set

data XY = XY
    { _x :: {-# UNPACK #-} !Int
    , _y :: {-# UNPACK #-} !Int
    } deriving (Eq,Read,Show)
makeLenses ''XY

instance Ord XY where
    (XY ax ay) `compare` (XY bx by) =
        case ay `compare` by of
            EQ → ax `compare` bx
            c → c
    (XY ax ay) <= (XY bx by)
      | ay == by = ax <= bx
      | otherwise = ay <= by
    (XY ax ay) < (XY bx by)
      | ay == by = ax < bx
      | otherwise = ay < by
    (XY ax ay) >= (XY bx by)
      | ay == by = ax >= bx
      | otherwise = ay >= by
    (XY ax ay) > (XY bx by)
      | ay == by = ax > bx
      | otherwise = ay > by

instance Monoid XY where
    mempty = XY 0 0
    (XY ax ay) `mappend` (XY bx by) = XY (ax + bx) (ay + by)

data Place =
    Bedrock
  | Floor
  deriving (Eq,Ord,Read,Show)

data Box = Box
    {   _width :: {-# UNPACK #-} !Int
    ,   _height :: {-# UNPACK #-} !Int
    ,   _places :: !(Map XY Place)
    } deriving (Eq,Ord,Read,Show)
makeLenses ''Box

type instance Index Box = XY
type instance IxValue Box = Place

instance (Contravariant f, Functor f) => Contains f Box where
    contains xy = places . contains xy
 
instance At Box where
    at xy = places . at xy

instance Functor f ⇒ Ixed f Box where
    ix xy f box =
        indexed f xy (Map.findWithDefault Bedrock xy (box ^. places))
        <&>
        (box &) . (places %~) . Map.insert xy

