-- Extensions {{{
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

import Control.Applicative ((<$>),(<*>),liftA2)
import Control.Lens -- {{{
    (Contains(..)
    ,Contravariant
    ,Index
    ,IndexedTraversal'
    ,(^.)
    ,(<&>)
    ,containsTest
    ,indexed
    ,makeLenses
    )
-- }}}

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse)

-- }}}

-- Types {{{

data XY = XY -- {{{
    { _x :: {-# UNPACK #-} !Int
    , _y :: {-# UNPACK #-} !Int
    } deriving (Eq,Read,Show)
makeLenses ''XY
-- }}}

data Wall = -- {{{
  H | V | NE | NW | SE | SW
  deriving (Bounded,Enum,Eq,Ord,Read,Show)
-- }}}

data Place = -- {{{
    Wall Wall
  | Floor
  | Rock
  deriving (Eq,Ord,Read,Show)
-- }}}

type Area = Map XY Place

data Bounds = Bounds -- {{{
    {   _width :: {-# UNPACK #-} !Int
    ,   _height ::{-# UNPACK #-} !Int
    } deriving (Eq,Ord,Read,Show)
makeLenses ''Bounds

type instance Index Bounds = XY
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

-- }}}

-- Functions {{{

boxOfXYs :: XY → Bounds → [XY] -- {{{
boxOfXYs (XY first_x first_y) (Bounds width height) =
    flip XY <$> [first_y..last_y] <*> [first_x..last_x]
  where
    last_x = first_x + width - 1
    last_y = first_y + height - 1
-- }}}

numberOfPlaces :: Bounds → Int -- {{{
numberOfPlaces = liftA2 (*) (^.width) (^.height)
-- }}}

room :: XY → Bounds → Area -- {{{
room (XY first_x first_y) (Bounds width height)
  | width < 2 || height < 2 = error $ "bounds for a room be at least two in both directions, not " ++ show width ++ " by " ++ show height
  | otherwise =
        Map.fromList
        $
        [
             (XY first_x first_y,Wall NW)
            ,(XY last_x first_y,Wall NE)
            ,(XY last_x last_y,Wall SW)
            ,(XY first_x last_y,Wall SE)
        ]
        ++
        ([first_x+1..last_x-1] >>= \x → [(XY x first_y,Wall H),(XY x last_y,Wall H)])
        ++
        ([first_y+1..last_y-1] >>= \y → [(XY first_x y,Wall V),(XY last_x y,Wall V)])
  where
    last_x = first_x + width
    last_y = first_y + height
    interior_width = width - 2
    interior_height = height - 2
-- }}}

translate :: XY → Area → Area -- {{{
translate = Map.mapKeysMonotonic . mappend
-- }}}

traverseArea :: XY → Bounds → IndexedTraversal' XY Area (Maybe Place) -- {{{
traverseArea offset bounds f area =
    traverse g (boxOfXYs offset bounds)
    <&>
    foldl' (flip ($)) area
  where
    g xy =
        (indexed f xy $ Map.lookup xy area)
        <&>
        maybe (Map.delete xy) (Map.insert xy)
-- }}}

-- }}}
