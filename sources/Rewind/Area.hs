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

data XY = XY
    { _x :: {-# UNPACK #-} !Int
    , _y :: {-# UNPACK #-} !Int
    } deriving (Eq,Ord,Read,Show)
makeLenses ''XY

data Place =
    Rock
  | Floor

data Area = Area
    {   _parent :: !(XY → Place)
    ,   _places :: !(Map XY Place)
    }
makeLenses ''Area

type instance Index Area = XY
type instance IxValue Area = Place

instance (Contravariant f, Functor f) => Contains f Area where
    contains = containsTest (\xy area → Map.member xy (area ^. places))

instance At Area where
    at xy f area =
        indexed f xy is_member
        <&>
        maybe (maybe area insert is_member) insert
      where
        is_member = area ^. places ^. to (Map.lookup xy)
        insert = (area &) . (places %~) . Map.insert xy

instance Functor f ⇒ Ixed f Area where
    ix xy f area =
        indexed f xy
            (fromMaybe
                (area ^. parent $ xy)
                (area ^. places ^. to (Map.lookup xy))
            )
        <&>
        (area &) . (places %~) . Map.insert xy
