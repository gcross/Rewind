{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rewind.Area where

import Control.Lens
    (At(..)
    ,Contains(..)
    ,Contravariant
    ,Index
    ,Ixed(..)
    ,IxValue
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
    Rock
  | Floor

data Area = Area
    {   _translation :: {-# UNPACK #-} !XY
    ,   _parent :: !(XY → Place)
    ,   _places :: !(Map XY Place)
    }
makeLenses ''Area

translate :: XY → Area → Area
translate xy = translation %~ (<> xy)

type instance Index Area = XY
type instance IxValue Area = Place

instance (Contravariant f, Functor f) => Contains f Area where
    contains = containsTest (\xy area → Map.member (area ^. translation <> xy) (area ^. places))

instance At Area where
    at xy f area =
        indexed f xy is_member
        <&>
        maybe (maybe area insert is_member) insert
      where
        abs_xy = area ^. translation <> xy
        is_member = area ^. places ^. to (Map.lookup abs_xy)
        insert = (area &) . (places %~) . Map.insert abs_xy

instance Functor f ⇒ Ixed f Area where
    ix xy f area =
        indexed f xy
            (fromMaybe
                (area ^. parent $ xy)
                (area ^. places ^. to (Map.lookup xy))
            )
        <&>
        (area &) . (places %~) . Map.insert abs_xy
      where
        abs_xy = area ^. translation <> xy


type instance Index SelectedArea = XY
type instance IxValue SelectedArea = Place

data SelectedArea = SelectedArea
    {   _selection :: Set XY
    ,   _area :: Area
    }
makeLenses ''SelectedArea

instance (Contravariant f, Functor f) => Contains f SelectedArea where
    contains xy = area . contains xy

instance At SelectedArea where
    at xy f select
      | Set.member abs_xy (select ^. selection) =
            at xy f (select ^. area)
            <&>
            (select &) . (area .~)
      | otherwise =
            indexed f xy Nothing
            <&>
            const select
      where
        abs_xy = select ^. area ^. translation <> xy

instance Functor f ⇒ Ixed f SelectedArea where
    ix xy f select
      | Set.member abs_xy (select ^. selection) =
            ix xy f (select ^. area)
            <&>
            (select &) . (area .~)
      | otherwise =
            indexed f xy (select ^. area ^. parent ^. to ($ abs_xy))
            <&>
            const select
      where
        abs_xy = select ^. area ^. translation <> xy
