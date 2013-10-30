-- Language extensions {{{
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

-- Imports {{{
import Control.Applicative ((<$>),(<*>))
import Control.Monad (msum)

import Control.Lens ((^.))

import Test.Framework (defaultMain,testGroup)
import Test.Framework.Providers.SmallCheck (testProperty)
import Test.SmallCheck (Property(..))
import Test.SmallCheck.Series (Serial(..),Series,getPositive)

import Rewind.Area
-- }}}

-- Types {{{

data BoundsAndIndex = BoundsAndIndex Bounds Int deriving (Eq,Ord,Read,Show)

data BoundsAndXY = BoundsAndXY Bounds XY deriving (Eq,Ord,Read,Show)

data BoundsAndXY2 = BoundsAndXY2 Bounds XY XY deriving (Eq,Ord,Read,Show)

-- }}}

-- Instances {{{

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

-- Functions {{{

xyForBounds :: Bounds → Series m XY -- {{{
xyForBounds (Bounds width height) =
    XY <$> (msum . map return $ [0..width-1]) <*> (msum . map return $ [0..height-1])
-- }}}

-- }}}

main = defaultMain -- {{{
    [testGroup "Rewind.Area" $ -- {{{
        [testGroup "xy ↔ i" $ -- {{{
            [testProperty "order preserving" $ \(BoundsAndXY2 bounds xy1 xy2) → -- {{{
                xy1 `compare` xy2 == xy2i bounds xy1 `compare` xy2i bounds xy2
             -- }}}
            ,testGroup "xy_i is an isomorphism" -- {{{
                [testProperty "forward direction" $ \(BoundsAndXY bounds xy) → -- {{{
                    (i2xy bounds . xy2i bounds) xy == xy
                 -- }}}
                ,testProperty "backward direction" $ \(BoundsAndIndex bounds i) → -- {{{
                    (xy2i bounds . i2xy bounds) i == i
                 -- }}}
                ]
             -- }}}
            ]
         -- }}}
        ]
     -- }}}
    ]
-- }}}
