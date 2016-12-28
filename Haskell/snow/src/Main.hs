{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad.Random
import System.Random

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.Pure.Game

data Snowflake = SF
  { _yPos :: Float
  , _xPos :: Float
  , _zPos :: Float
  , _ySpeed :: Float -- Pixels per second
  , _xSpeed :: Float -- Pixels per second
  , _size :: Float   -- [1..10]
  } deriving (Show, Eq)

makeLenses ''Snowflake


fs :: Int
fs = 5

square :: Float -> Picture
square a = Polygon
  [(0.0,0.0),(a,0),(a,a),(0,a)]

type Model = ((Float, Float), [Snowflake])

oneFlake:: [Snowflake]
oneFlake = [SF 0.0 0.0 1.0 10.0 1.0 1]

someFlakes :: StdGen -> Int -> [Snowflake]
someFlakes gen n = evalRand (flakes n) gen
  where
    flakes 0 = return []
    flakes n = do
      x <- getRandomR (-256, 256)
      y <- getRandomR (-256, 256)
      z <- getRandomR (0.1,1)
      size <- getRandomR (1,4)
      (SF y x z 0 0 size :) <$> flakes (n-1)


renderFlake :: Snowflake -> Picture
renderFlake sf
  = Translate (sf ^. xPos) (sf ^. yPos)
  $ Color white
  $ Scale (sf ^. zPos) (sf ^. zPos)
--  $ Scale 0.1 0.1
  -- $ Text "*"
  $ square 3

renderModel :: Model -> Picture
renderModel = Pictures . map renderFlake . snd

stepFlake :: (Float, Float) -> Float -> Snowflake -> Snowflake
stepFlake (fx, fy) time sf
  = sf
  & xSpeed %~ (\v -> if sf ^. yPos < -256 then 0 else v)
  & ySpeed %~ (\v -> if sf ^. yPos < -256 then 0 else v)
  & ySpeed +~ 9.81 * time
  & ySpeed +~ (fy / sf ^. size) * time
  & xSpeed +~ (fx / sf ^. size) * time
  & yPos %~ (\p -> if p < -256 then 256 else p)
  & yPos -~ (time * sf ^. ySpeed * sf ^. zPos)
  & xPos +~ (time * sf ^. xSpeed * sf ^. zPos)

step :: Float -> Model -> Model
step time m = (_1.both .~ 0.0)
            . over (_2 . mapped) (stepFlake (m ^. _1) time)
            $ m

events :: Event -> Model -> Model
events (EventKey (SpecialKey KeyLeft) Down _ _) = _1._1 -~ 1000
events (EventKey (SpecialKey KeyRight) Down _ _) = _1._1 +~ 1000
events (EventKey (SpecialKey KeyUp) Down _ _) = _1._2 -~ 1000
events (EventKey (SpecialKey KeyDown) Down _ _) = _1._2 +~ 1000
events _ = id

disp :: Display
disp = InWindow "Snowflakes" (512,512) (300,300)

main :: IO ()
main = do
  gen <- getStdGen
  play disp (light blue) 30 ((0,0), someFlakes gen 3000) renderModel events step
