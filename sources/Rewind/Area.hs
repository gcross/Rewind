{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rewind.Area where

import Control.Exception (Exception,throw)
import Control.Lens
    (Contravariant(..)
    ,Index
    ,IndexedTraversal'
    ,Iso'
    ,Ixed(..)
    ,IxValue
    ,Getter
    ,Lens
    ,(^.)
    ,(<&>)
    ,(%~)
    ,(.~)
    ,(&)
    ,from
    ,indexed
    ,iso
    ,lens
    ,makeLenses
    ,to
    ,view
    )

import Data.Functor (Functor(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Data.Word

import TypeLevel.Number.Nat (Nat(..))

data InvalidCoordinateException =
    XCoordinateTooSmall Int
  | YCoordinateTooSmall Int
  | XCoordinateTooLarge Int Int
  | YCoordinateTooLarge Int Int
  | InvalidFlattenedCoordinate Int
  deriving (Typeable)
instance Show InvalidCoordinateException where
    show (XCoordinateTooSmall x) = "x coordinate (" ++ show x ++ ") is too small"
    show (YCoordinateTooSmall y) = "y coordinate (" ++ show y ++ ") is too small"
    show (XCoordinateTooLarge x width) = "x coordinate (" ++ show x ++ ") is too large (>= " ++ show width ++ ")"
    show (YCoordinateTooLarge y height) = "y coordinate (" ++ show y ++ ") is too large (>= " ++ show height ++ ")"
    show (InvalidFlattenedCoordinate i) = "flat coordinate (" ++ show i ++ ") is invalid"
instance Exception InvalidCoordinateException

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
    Wall
  | Floor

data Number n where
    Number :: Nat n ⇒ Number n

instance ∀ n. Nat n ⇒ Nat (Number n) where
    toInt Number = toInt (undefined :: n)

data Bounds w h = Bounds 
makeLenses ''Bounds

height :: ∀ w h1 h2. (Nat h1, Nat h2) ⇒ Lens (Bounds w h1) (Bounds w h2) (Number h1) (Number h2)
height = lens (\_ → Number) (\_ _ → Bounds)

width :: ∀ w1 w2 h. (Nat w1, Nat w2) ⇒  Lens (Bounds w1 h) (Bounds w2 h) (Number w1) (Number w2)
width = lens (\_ → Number) (\_ _ → Bounds)

asInt :: Nat n ⇒ Getter (Number n) Int
asInt = to toInt

data Area w h = Area
    {   _parent :: Int → Place
    ,   _places :: !(IntMap Place)
    }
makeLenses ''Area

changeAreaWH :: Area w1 h1 → Area w2 h2
changeAreaWH (Area a b) = Area a b

area_iso :: Iso' (Area w1 h1) (Area w2 h2)
area_iso = iso changeAreaWH changeAreaWH

bounds :: ∀ w1 h1 w2 h2. (Nat w1, Nat h1, Nat w2, Nat h2) ⇒ Lens (Area w1 h1) (Area w2 h2) (Bounds w1 h1) (Bounds w2 h2)
bounds = lens a2b b2a
  where
    a2b _ = Bounds
    b2a area@(Area parent places) new_bounds =
        Area (parent . (view . from $ old_i_new_i))
             (IntMap.mapKeys (view old_i_new_i :: Int → Int) places)
      where
        old_bounds = area ^. bounds
        old_xy_i = xy_i old_bounds
        new_xy_i = xy_i new_bounds
        old_i_new_i = from old_xy_i . new_xy_i

xy_i :: ∀ w h. (Nat w, Nat h) ⇒ Bounds w h → Iso' XY Int
xy_i bounds = iso forward backward
  where
    forward (XY x y)
      | x < 0 = throw $ XCoordinateTooSmall x
      | y < 0 = throw $ YCoordinateTooSmall y
      | x >= w = throw $ XCoordinateTooLarge x w
      | y >= h = throw $ YCoordinateTooLarge y h
      | otherwise = x + y*w
    {-# INLINE forward #-}

    backward = uncurry XY . flip divMod w
    {-# INLINE backward #-}

    w = bounds ^. width . asInt
    h = bounds ^. height . asInt
{-# INLINE xy_i #-}

type instance Index (Area w h) = XY
type instance IxValue (Area w h) = Place

instance (Functor f, Nat w, Nat h) ⇒ Ixed f (Area w h) where
    ix xy f level =
        indexed f xy
            (fromMaybe
                (level ^. parent $ i)
                (level ^. places ^. to (IntMap.lookup i))
            )
        <&>
        \place → (places %~ IntMap.insert i place) level
      where i = xy ^. xy_i (level ^. bounds)

area_traversal :: (Nat w, Nat h) ⇒ IndexedTraversal' XY (Area w h) Place
area_traversal f area =
    IntMap.traverseWithKey (indexed f . (view . from . xy_i $ area ^. bounds)) (area ^. places)
    <&>
    (area &) . (places .~)

-- NOTE:  Create lenses, using the parent field to avoid copying.
