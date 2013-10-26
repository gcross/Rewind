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
    ,(^.)
    ,(<&>)
    ,(%~)
    ,(.~)
    ,(&)
    ,indexed
    ,iso
    ,makeLenses
    ,to
    )

import Data.Functor (Functor(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
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
    {   _width :: {-# UNPACK #-} !Int
    ,   _height ::{-# UNPACK #-} !Int
    }
makeLenses ''Bounds

data Area = Area
    {   _bounds :: Bounds
    ,   _places :: !(IntMap Place)
    }
makeLenses ''Area

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
                (throw $ InvalidFlattenedCoordinate i)
                (level ^. places ^. to (IntMap.lookup i))
            )
        <&>
        \place → (places %~ IntMap.insert i place) level
      where i = xy2i (level ^. bounds) xy

area_traversal :: IndexedTraversal' XY Area Place
area_traversal f area =
    IntMap.traverseWithKey (indexed f . i2xy (area ^. bounds)) (area ^. places)
    <&>
    (area &) . (places .~)
