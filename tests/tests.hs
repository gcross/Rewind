-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Applicative ((<$>),(<*>))
import Control.Monad (msum)
import Control.Lens ((&),(.~),(^.),contains,to)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (mapMaybe)

import Test.Framework (defaultMain,testGroup)
import qualified Test.Framework.Providers.QuickCheck2 as Quick
import qualified Test.Framework.Providers.SmallCheck as Small
import Test.QuickCheck.Arbitrary (Arbitrary(..),arbitraryBoundedEnum)
import Test.QuickCheck.Gen (choose,vectorOf)
import Test.SmallCheck (Property(..))
import Test.SmallCheck.Series (Serial(..),Series,getPositive)

import Text.Printf (printf)

import Rewind.Area
-- }}}

-- Types {{{

data AreaAndXY = AreaAndXY Area XY

data BoundsAndIndex = BoundsAndIndex Bounds Int deriving (Eq,Ord,Read,Show)

data BoundsAndXY = BoundsAndXY Bounds XY deriving (Eq,Ord,Read,Show)

data BoundsAndXY2 = BoundsAndXY2 Bounds XY XY deriving (Eq,Ord,Read,Show)

-- }}}

-- Instances {{{

-- Arbitrary {{{

instance Arbitrary Area where -- {{{
    arbitrary = do
        bounds ← arbitrary
        parent ← arbitrary
        places ←
            IntMap.fromAscList
            .
            mapMaybe (\(i,maybe_place) → (i,) <$> maybe_place)
            .
            zip [0..]
            <$>
            vectorOf (numberOfPlaces bounds) arbitrary
        return $ Area bounds parent places
-- }}}

instance Arbitrary AreaAndXY where -- {{{
    arbitrary = do
        area ← arbitrary
        x ← choose (0,area^.width-1)
        y ← choose (0,area^.height-1)
        return $ AreaAndXY area (XY x y)
-- }}}

instance Arbitrary Bounds where -- {{{
    arbitrary = Bounds <$> choose (1,10) <*> choose (1,10)
-- }}}

instance Arbitrary Place where arbitrary = arbitraryBoundedEnum

-- }}}

-- Serial {{{

instance Monad m ⇒ Serial m Bounds where -- {{{
    series = Bounds <$> (getPositive <$> series) <*> (getPositive <$> series)
-- }}}

instance Monad m ⇒ Serial m BoundsAndIndex where -- {{{
    series = do
        bounds@(Bounds width height) ← series
        BoundsAndIndex <$> return bounds <*> (msum . map return $ [0..width*height-1])
-- }}}

instance Monad m ⇒ Serial m BoundsAndXY where -- {{{
    series = do
        bounds ← series
        BoundsAndXY <$> return bounds <*> xyForBounds bounds
-- }}}

instance Monad m ⇒ Serial m BoundsAndXY2 where -- {{{
    series = do
        bounds ← series
        BoundsAndXY2 <$> return bounds <*> xyForBounds bounds <*> xyForBounds bounds
-- }}}

-- }}}

instance Show AreaAndXY where -- {{{
    show (AreaAndXY area xy) =
        printf "AreaAndXY (Area %s _ %s) %s"
            (show (area^.bounds))
            (show (area^.places))
            (show xy)
-- }}}

-- }}}

-- Functions {{{

xyForBounds :: Bounds → Series m XY -- {{{
xyForBounds (Bounds width height) =
    XY <$> (msum . map return $ [0..width-1]) <*> (msum . map return $ [0..height-1])
-- }}}

-- }}}

main = defaultMain -- {{{
    [testGroup "Rewind.Area" $ -- {{{
        [testGroup "xy ↔ i" $ -- {{{
            [Small.testProperty "order preserving" $ \(BoundsAndXY2 bounds xy1 xy2) → -- {{{
                xy1 `compare` xy2 == xy2i bounds xy1 `compare` xy2i bounds xy2
             -- }}}
            ,testGroup "xy_i is an isomorphism" -- {{{
                [Small.testProperty "forward direction" $ \(BoundsAndXY bounds xy) → -- {{{
                    (i2xy bounds . xy2i bounds) xy == xy
                 -- }}}
                ,Small.testProperty "backward direction" $ \(BoundsAndIndex bounds i) → -- {{{
                    (xy2i bounds . i2xy bounds) i == i
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "Area instances" $ -- {{{
            [testGroup "Contains" $ -- {{{
                [Quick.testProperty "get" $ \(AreaAndXY area xy) → -- {{{
                    area ^. contains xy
                    ==
                    IntMap.member (xy2i area xy) (area^.places)
                 -- }}}
                ,Quick.testProperty "set (False)" $ \(AreaAndXY area xy) → -- {{{
                    (area & contains xy .~ False) ^. contains xy == False
                 -- }}}
                ,Quick.testProperty "set (True)" $ \(AreaAndXY area xy) → -- {{{
                    (area & contains xy .~ True) ^. contains xy == True
                 -- }}}
                ,Quick.testProperty "set (parent)" $ \(AreaAndXY area xy) → -- {{{
                    let i = xy2i area xy
                    in (area
                        & contains xy .~ False
                        & contains xy .~ True
                       ) ^. places ^. to (IntMap.lookup i) == Just (area^.parent $ i)
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ]
-- }}}
