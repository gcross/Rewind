-- Extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Rewind.Area where

-- Imports {{{
import Control.Applicative (liftA2)
import Control.Exception (Exception,throw)
import Control.Lens
    (At(..)
    ,Contains(..)
    ,Contravariant(..)
    ,Getter
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
    ,containsTest
    ,from
    ,indexed
    ,iso
    ,lens
    ,makeLenses
    ,to
    ,toListOf
    ,view
    )

import Data.Functor (Functor(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Monoid (Monoid(..),(<>))
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Data.Word

import GHC.Generics (Generic)
-- }}}

-- Exceptions {{{

data InvalidCoordinateException = -- {{{
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
-- }}}

-- }}}

-- Types {{{

data XY = XY -- {{{
    { _x :: {-# UNPACK #-} !Int
    , _y :: {-# UNPACK #-} !Int
    } deriving (Eq,Generic,Read,Show)
makeLenses ''XY
-- }}}

data Place = -- {{{
    Wall
  | Floor
  deriving (Bounded,Enum,Eq,Generic,Ord,Read,Show)
-- }}}

data Bounds = Bounds -- {{{
    {   width_ :: {-# UNPACK #-} !Int
    ,   height_ ::{-# UNPACK #-} !Int
    } deriving (Eq,Generic,Ord,Read,Show)
makeLenses ''Bounds

type instance Index Bounds = XY
-- }}}

data Area = Area -- {{{
    {   bounds_ :: Bounds
    ,   _parent :: Int → Place
    ,   _places :: !(IntMap Place)
    } deriving Generic
makeLenses ''Area

type instance Index Area = XY
type instance IxValue Area = Place
-- }}}

-- }}}

-- Classes {{{

class HasBounds α where -- {{{
    width :: Getter α Int
    height :: Getter α Int
-- }}}

-- }}}

-- Instances {{{

-- XY {{{

instance Monoid XY where -- {{{
    mempty = XY 0 0
    (XY ax ay) `mappend` (XY bx by) = XY (ax + bx) (ay + by)
-- }}}

instance Ord XY where -- {{{
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
-- }}}

-- }}}

-- Bounds {{{

instance HasBounds Bounds where -- {{{
    width = to width_
    height = to height_
-- }}}

instance (Contravariant f, Functor f) => Contains f Bounds where -- {{{
    contains = containsTest inBounds
-- }}}

-- }}}

-- Area {{{

instance At Area where -- {{{
    at xy f area =
        indexed f xy is_member
        <&>
        maybe
            (case is_member of
                Nothing → area
                Just _ → area & places %~ IntMap.delete i
            )
            ((area &) . (places %~) . IntMap.insert i)
      where
        i = xy2i area xy
        is_member = area ^. places ^. to (IntMap.lookup i)
-- }}}

instance Eq Area where -- {{{
    a == b = toListOf full_area_traversal a == toListOf full_area_traversal b
-- }}}

instance Functor f ⇒ Contains f Area where -- {{{
    contains xy f area =
        indexed f xy is_member
        <&>
        \flag → area &
            case (is_member,flag) of
                (False,True) → places %~ IntMap.insert i (area ^. parent $ i)
                (True,False) → places %~ IntMap.delete i
                _ → id
      where
        i = xy2i area xy
        is_member = IntMap.member i (area ^. places)
-- }}}

instance HasBounds Area where -- {{{
    width = bounds . width
    height = bounds . height
-- }}}

instance Functor f ⇒ Ixed f Area where -- {{{
    ix xy f area =
        indexed f xy
            (fromMaybe
                (area ^. parent $ i)
                (area ^. places ^. to (IntMap.lookup i))
            )
        <&>
        \place → (places %~ IntMap.insert i place) area
      where i = xy2i (area ^. bounds) xy
-- }}}

instance Show Area where -- {{{
    show area = "Area " ++ show (area^.bounds) ++ " _ " ++ show (area^.places)
-- }}}

-- }}}

-- }}}

-- Functions {{{

inBounds :: XY → Bounds → Bool -- {{{
inBounds (XY x y) bounds
  | x < 0                 = False
  | x >= bounds ^. width  = False
  | y < 0                 = False
  | y >= bounds ^. height = False
  | otherwise = True
-- }}}

i2xy :: HasBounds α ⇒ α → Int → XY -- {{{
i2xy bounds = uncurry (flip XY) . flip divMod (bounds ^. width)
{-# INLINE i2xy #-}
-- }}}

numberOfPlaces :: HasBounds α ⇒ α → Int -- {{{
numberOfPlaces = liftA2 (*) (^.width) (^.height)
-- }}}

xy2i :: HasBounds α ⇒ α → XY → Int -- {{{
xy2i bounds (XY x y)
  | x < 0 = throw $ XCoordinateTooSmall x
  | y < 0 = throw $ YCoordinateTooSmall y
  | x >= bounds ^. width = throw $ XCoordinateTooLarge x (bounds ^. width)
  | y >= bounds ^. height = throw $ YCoordinateTooLarge y (bounds ^. height)
  | otherwise = x + y * bounds ^. width
{-# INLINE xy2i #-}
-- }}}

-- }}}

-- Lenses, etc. {{{

bounds :: Getter Area Bounds -- {{{
bounds = to bounds_
-- }}}

full_area_traversal :: IndexedTraversal' XY Area Place -- {{{
full_area_traversal f area =
    traverse g [0..numberOfPlaces area - 1]
    <&>
    (area &) . (places .~) . IntMap.fromAscList
  where
    g i = fmap (i,)
          .
          apply (my_i2xy i)
          $
          fromMaybe
              (area ^. parent $ i)
              (area ^. places ^. to (IntMap.lookup i))
    apply = indexed f
    my_i2xy = i2xy area
-- }}}

select :: XY → Bounds → Lens' Area Area -- {{{
select offset selected_bounds =
    lens (\area →
        Area selected_bounds
             (
                (area^.)
                 .
                 ix
                 .
                 (<> offset)
                 .
                 i2xy selected_bounds
             )
             mempty
        )
        (\area selected_area →
            (area &)
            .
            (places .~)
            .
            flip mappend (area^.places) -- mappend is left-biased
            .
            IntMap.mapKeys (
                xy2i (area^.bounds)
                .
                (<> offset)
                .
                i2xy (selected_area^.bounds) {- NOTE: This should be the same
                                                      as the selected bound, but
                                                      if it is not (which is a
                                                      contract violation) then
                                                      using this value will at
                                                      least be self-consistent.
                                              -}
            )
            .
            (^.places)
            $
            selected_area
        )
-- }}}

used_area_traversal :: IndexedTraversal' XY Area Place -- {{{
used_area_traversal f area =
    IntMap.traverseWithKey (indexed f . i2xy area) (area ^. places)
    <&>
    (area &) . (places .~)
-- }}}

xy_i :: HasBounds α ⇒ α → Iso' XY Int -- {{{
xy_i bounds = iso (xy2i bounds) (i2xy bounds)
-- }}}

-- }}}
