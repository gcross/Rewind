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

data XY = XY
    { _x :: {-# UNPACK #-} !Int
    , _y :: {-# UNPACK #-} !Int
    } deriving (Eq,Ord,Read,Show)
makeLenses ''XY

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
