-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Applicative ((<$>),(<*>))
import Control.Monad (msum,unless)
import Control.Lens
    ((&)
    ,(.~)
    ,(%~)
    ,(^.)
    ,(^@..)
    ,allOf
    ,at
    ,contains
    ,iact
    ,imapMOf_
    ,ix
    ,to
    )

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')
import Data.Maybe (fromMaybe,mapMaybe)
import Data.Monoid ((<>))

import Debug.Trace

import Test.Framework (defaultMain,testGroup)
import qualified Test.Framework.Providers.QuickCheck2 as Quick
import qualified Test.Framework.Providers.SmallCheck as Small
import Test.HUnit ((@?=),(@=?),assertBool)
import Test.QuickCheck.Arbitrary (Arbitrary(..),arbitraryBoundedEnum)
import Test.QuickCheck.Gen (choose,listOf,oneof,vectorOf)
import Test.QuickCheck.Property (morallyDubiousIOProperty)
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

instance Arbitrary Place where -- {{{
    arbitrary = oneof
        [return Floor
        ,Wall <$> arbitrary
        ]
-- }}}

instance Arbitrary Wall where arbitrary = arbitraryBoundedEnum

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

echo :: Show α ⇒ α → α -- {{{
echo x = trace (show x) x
-- }}}

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
            [testGroup "At" $ -- {{{
                [Quick.testProperty "get" $ \(AreaAndXY area xy) → -- {{{
                    let i = xy2i area xy
                    in area ^. at xy
                       ==
                       IntMap.lookup i (area^.places)
                 -- }}}
                ,Quick.testProperty "set (Nothing)" $ \(AreaAndXY area xy) → -- {{{
                    (area & at xy .~ Nothing) ^. at xy == Nothing
                 -- }}}
                ,Quick.testProperty "set (Just x)" $ \(AreaAndXY area xy) place → -- {{{
                    (area & at xy .~ Just place) ^. at xy == Just place
                 -- }}}
                ]
             -- }}}
            ,testGroup "Contains" $ -- {{{
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
            ,testGroup "Ix" $ -- {{{
                [Quick.testProperty "get" $ \(AreaAndXY area xy) → -- {{{
                    let i = xy2i area xy
                    in area ^. ix xy
                       ==
                       fromMaybe (area^.parent $ i) (IntMap.lookup i (area^.places))
                 -- }}}
                ,Quick.testProperty "set" $ \(AreaAndXY area xy) place → -- {{{
                    (area & ix xy .~ place) ^. ix xy == place
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,testGroup "Area traversals" -- {{{
            [testGroup "full" -- {{{
                [Quick.testProperty "correct indices" $ \(AreaAndXY area xy) → -- {{{
                    map fst (area ^@.. full_area_traversal) == map (i2xy area) [0..numberOfPlaces area-1]
                 -- }}}
                ,Quick.testProperty "correct values" $ \area → morallyDubiousIOProperty $ -- {{{
                    (flip (imapMOf_ full_area_traversal) area $ \xy p → let i = xy2i area xy in
                        p @?= fromMaybe (area^.parent $ i) (IntMap.lookup i (area^.places))
                    ) >> return True
                 -- }}}
                ]
             -- }}}
            ,testGroup "used" -- {{{
                [Quick.testProperty "correct indices" $ \(AreaAndXY area xy) → -- {{{
                    map fst (area ^@.. used_area_traversal) == (map (i2xy area) . IntMap.keys $ area^.places)
                 -- }}}
                ,Quick.testProperty "correct values" $ \area → morallyDubiousIOProperty $ -- {{{
                    (flip (imapMOf_ used_area_traversal) area $ \xy p → let i = xy2i area xy in
                        p @?= fromMaybe (area^.parent $ i) (IntMap.lookup i (area^.places))
                    ) >> return True
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ,Quick.testProperty "select" $ do -- {{{
            area ← arbitrary
            offset ←
                XY <$> choose (0,area^.width-1)
                   <*> choose (0,area^.height-1)
            selected_bounds ←
                Bounds <$> choose (1,area^.width-offset^.x)
                       <*> choose (1,area^.height-offset^.y)
            edits ← listOf $
                (,) <$> (XY <$> choose (0,selected_bounds^.width-1)
                            <*> choose (0,selected_bounds^.height-1)
                        )
                    <*> arbitrary
            let correct_new_area =
                    foldl' (flip $ \(xy,place) → ix (offset <> xy) .~ place) area edits
                new_area = area & select offset selected_bounds %~
                    flip (foldl' (flip $ \(xy,place) → ix xy .~ place)) edits
            morallyDubiousIOProperty $ do
                unless (correct_new_area == new_area) $ do
                    putStrLn $ ""
                    putStrLn $ "offset = " ++ show offset
                    putStrLn $ "selected_bounds = " ++ show selected_bounds
                    putStrLn $ "edits = " ++ show edits
                    putStrLn $ "correct_new_area = " ++ show correct_new_area
                    putStrLn $ "new_area = " ++ show new_area
                return True
         -- }}}
        ,Quick.testProperty "room" $ do
            area ← room <$> (Bounds <$> choose (2,5) <*> choose (2,5))
            let last_x = area^.width - 1
                last_y = area^.height - 1
                wall_width = area^.width - 2
                wall_height = area^.height - 2
            morallyDubiousIOProperty $ do
                Wall NW @=? area ^. ix (XY 0 0)
                Wall NE @=? area ^. ix (XY last_x 0)
                Wall SW @=? area ^. ix (XY last_x last_y)
                Wall SE @=? area ^. ix (XY 0 last_y)
                assertBool "North" $ allOf (select (XY 1 0) (Bounds wall_width 1) . full_area_traversal) (== Wall H) area
                assertBool "East" $ allOf (select (XY last_x 1) (Bounds 1 wall_height) . full_area_traversal) (== Wall V) area
                assertBool "South" $ allOf (select (XY 1 last_y) (Bounds wall_width 1) . full_area_traversal) (== Wall H) area
                assertBool "West" $ allOf (select (XY 0 1) (Bounds 1 wall_height) . full_area_traversal) (== Wall V) area
                assertBool "Interior" $ allOf (select (XY 1 1) (Bounds wall_width wall_height) . full_area_traversal) (== Floor) area
                return True
        ]
     -- }}}
    ]
-- }}}
