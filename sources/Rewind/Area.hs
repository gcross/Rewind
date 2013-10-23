{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rewind.Area where

import Control.Lens
    (Index
    ,Ixed(..)
    ,IxValue
    ,(&)
    ,(<&>)
    ,(^.)
    ,(%~)
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

instance Functor f ⇒ Ixed f Area where
    ix xy f area =
        indexed f xy
            (fromMaybe
                (area ^. parent $ xy)
                (area ^. places ^. to (Map.lookup xy))
            )
        <&>
        (area &) . (places %~) . Map.insert xy
