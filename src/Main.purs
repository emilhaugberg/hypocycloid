module Main where

import Prelude (Unit, bind, map, void, show, ($), (*), (/), (+), (-), (<<<))
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.Eff.Timer (TIMER, setInterval)
import Control.Monad.Eff.Console (log)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Semiring (class Semiring)
import Data.Show (class Show)
import Graphics.Canvas (CANVAS, Context2D, arc, beginPath, clearRect, fill, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, stroke)
import Math as Math

type Line       = { start :: Angle, end :: Angle }
type Position   = Tuple Number Number
type LineCoords = Tuple Position Position

newtype Angle   = Angle Number
derive newtype instance semiringAngle :: Semiring Angle
derive newtype instance showgAngle :: Show Angle

width  = 600.0
height = 600.0

radius = 250.0
dotRadius = 10.0

space :: Angle
space = Angle (Math.pi / 2.0 / 4.0)

points :: Array Angle
points = map ((*) space <<< Angle) [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]

lines :: Array Line
lines  = map ptl points
  where
    ptl p = { start: p, end: p + Angle Math.pi }

linePositions :: Line -> LineCoords
linePositions lo = Tuple start end
  where
    end           = Tuple (xCoord lo.end)   (yCoord lo.end)
    start         = Tuple (xCoord lo.start) (yCoord lo.start)
    xCoord        = calcCoord Math.cos
    yCoord        = calcCoord Math.sin
    calcCoord f (Angle a) = ((f a) * radius) + 300.0

drawCircle :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawCircle ctx = do
  let a = { x: width / 2.0, y: height / 2.0, r: radius, start: 0.0, end: Math.pi * 2.0 }
  beginPath ctx
  arc ctx a
  setFillStyle "#F44336" ctx
  fill ctx

drawLines :: forall e. Array Line -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawLines lines ctx = foreachE lines (\lo -> void $ drawLine lo ctx)

drawLine :: forall e. Line -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawLine lo ctx = do
  let coords = linePositions lo
  let begin  = fst coords
  let end    = snd coords

  beginPath ctx
  moveTo ctx (fst begin) (snd begin)
  lineTo ctx (fst end)   (snd end)
  stroke ctx

drawDot :: forall e. Angle -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawDot (Angle ang) ctx = do
  let dot = calculateDotPos (Angle ang)
  let a = { x: fst dot, y: snd dot, r: dotRadius, start: 0.0, end: Math.pi * 2.0 }
  beginPath ctx
  arc ctx a
  setFillStyle "white" ctx
  fill ctx

calculateDotPos :: Angle -> Position
calculateDotPos (Angle ang) =
  Tuple
  (calculatePosition Math.cos radius)
  (calculatePosition Math.sin 0.0)
  where
    calculatePosition f r = (f ang) * r + 300.0

-- move :: Angle -> Angle
-- move (Angle ang) = Angle (ang + (Math.pi / 10.0))

-- startingAngles :: Array Angle
-- startingAngles = map Angle [Math.pi / 2.0, Math.pi, Math.pi / 4.0, Math.pi / 5.0]

startingdots :: Array Angle
startingdots = map (\l -> l.start) lines

-- main :: forall e. (Partial) => Eff (ref :: REF, canvas :: CANVAS, timer :: TIMER | e) Unit
main = void $ do
  Just canvas <- getCanvasElementById "canvas"
  ctx         <- getContext2D canvas
  -- dots        <- newRef $ startingAngles

  log $ show startingdots
  -- setInterval 100 $ void $ do
  --   clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}
  --   modifyRef dots (map move)
  --   dots' <- readRef dots
  --   drawCircle ctx
  --   drawLines lines ctx
  --   foreachE dots' \d -> void $ drawDot d ctx
