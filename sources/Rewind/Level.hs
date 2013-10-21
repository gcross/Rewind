{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Rewind.Level where

import Control.Exception (Exception,throw)
import Control.Lens
    (Contravariant(..)
    ,Index
    ,Ixed(..)
    ,IxValue
    ,(^.)
    ,(<&>)
    ,(%~)
    ,indexed
    ,makeLenses
    ,to
    )

import Data.Functor (Functor(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
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
    } deriving (Eq,Ord,Read,Show)
makeLenses ''XY

data Place =
    Wall
  | Floor

data Level = Level
    {   _width :: {-# UNPACK #-} !Int
    ,   _height ::{-# UNPACK #-} !Int
    ,   _places :: !(IntMap Place)
    }
makeLenses ''Level

xy2i :: Level → XY → Int
xy2i places (XY x y)
  | x < 0 = throw $ XCoordinateTooSmall x
  | y < 0 = throw $ YCoordinateTooSmall y
  | x >= places ^. width = throw $ XCoordinateTooLarge x (places ^. width)
  | y >= places ^. height = throw $ YCoordinateTooLarge y (places ^. height)
  | otherwise = x + y * places ^. width

type instance Index Level = XY
type instance IxValue Level = Place

instance Functor f ⇒ Ixed f Level where
    ix xy f level =
        indexed f xy
            (fromMaybe
                (throw $ InvalidFlattenedCoordinate i)
                (level ^. places ^. to (IntMap.lookup i))
            )
        <&>
        \place → (places %~ IntMap.insert i place) level
      where i = xy2i level xy
