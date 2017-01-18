module Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Timer
import Data.Identity
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Graphics.Canvas
import Math as Math

type Line       = { start :: Number, end :: Number }
type Position   = Tuple Number Number
type Radian     = Number
type LineCoords = Tuple Position Position

newtype Angle   = Angle Number

width  = 600.0
height = 600.0

radius = 250.0
dotRadius = 10.0

space :: Number
space = Math.pi / 2.0 / 4.0

points :: Array Number
points = map ((*) space) [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]

lines :: Array Line
lines  = map ptl points
  where
    ptl p = { start: p, end: p + Math.pi }

linePositions :: Line -> LineCoords
linePositions lo = Tuple end start
  where
    end           = Tuple (xCoord lo.end)   (yCoord lo.end)
    start         = Tuple (xCoord lo.start) (yCoord lo.start)
    xCoord        = calcCoord Math.cos
    yCoord        = calcCoord Math.sin
    calcCoord f c = ((f c) * radius) + 300.0

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
  ((Math.cos (ang)) * 0.0 + 300.0)
  ((Math.sin (ang)) * radius + 300.0)

move :: Angle -> Angle
move (Angle ang) = Angle (ang + (Math.pi / 16.0))

main :: forall e. (Partial) => Eff (ref :: REF, canvas :: CANVAS, timer :: TIMER | e) Unit
main = void $ do
  Just canvas <- getCanvasElementById "canvas"
  ctx         <- getContext2D canvas

  exampleDot  <- newRef $ Angle Math.pi

  setInterval 100 $ void $ do
    clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}
    drawCircle ctx
    drawLines lines ctx
    modifyRef exampleDot move
    dot <- readRef exampleDot
    drawDot dot ctx
