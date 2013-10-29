{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    ,Lens'
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
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)
import Data.Word

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

data Bounds = Bounds
    {   _bounds_width :: {-# UNPACK #-} !Int
    ,   _bounds_height ::{-# UNPACK #-} !Int
    }
makeLenses ''Bounds

class HasBounds α where
    width :: Lens' α Int
    height :: Lens' α Int

instance HasBounds Bounds where
    width = bounds_width
    height = bounds_height

data Area = Area
    {   bounds_ :: Bounds
    ,   _parent :: Int → Place
    ,   _places :: !(IntMap Place)
    }
makeLenses ''Area

instance HasBounds Area where
    width = bounds . bounds_width
    height = bounds . bounds_height

bounds :: Lens' Area Bounds
bounds = lens bounds_ setBounds
  where
    setBounds (Area old_bounds old_parent old_places) new_bounds = Area new_bounds new_parent new_places
      where
        old_xy_i = xy_i old_bounds :: Iso' XY Int
        new_xy_i = xy_i new_bounds :: Iso' XY Int
        old_i_to_new_i = from old_xy_i . new_xy_i
        new_parent = old_parent . view (from old_i_to_new_i)
        new_places =
            IntMap.fromAscList
            .
            mapMaybe (\(i,p) →
                let xy@(XY x y) = i ^. from old_xy_i
                in if x < old_bounds ^. width && y < old_bounds ^. height
                    then Just (xy2i new_bounds xy,p) -- not sure why the iso doesn't work here
                    else Nothing
            )
            .
            IntMap.toAscList
            $
            old_places

xy2i :: Bounds → XY → Int
xy2i bounds (XY x y)
  | x < 0 = throw $ XCoordinateTooSmall x
  | y < 0 = throw $ YCoordinateTooSmall y
  | x >= bounds ^. width = throw $ XCoordinateTooLarge x (bounds ^. width)
  | y >= bounds ^. height = throw $ YCoordinateTooLarge y (bounds ^. height)
  | otherwise = x + y * bounds ^. width
{-# INLINE xy2i #-}

i2xy :: Bounds → Int → XY
i2xy bounds = uncurry XY . flip divMod (bounds ^. width)
{-# INLINE i2xy #-}

type instance Index Area = XY
type instance IxValue Area = Place

instance Functor f ⇒ Ixed f Area where
    ix xy f level =
        indexed f xy
            (fromMaybe
                (level ^. parent $ i)
                (level ^. places ^. to (IntMap.lookup i))
            )
        <&>
        \place → (places %~ IntMap.insert i place) level
      where i = xy2i (level ^. bounds) xy

used_area_traversal :: IndexedTraversal' XY Area Place
used_area_traversal f area =
    IntMap.traverseWithKey (indexed f . i2xy (area ^. bounds)) (area ^. places)
    <&>
    (area &) . (places .~)
