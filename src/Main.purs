module Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Graphics.Canvas
import Math as Math

type LineObj = { start :: Number, end :: Number }

width  = 600.0
height = 600.0

radius = 250.0

type Radian = Number
type LineCoords = Tuple (Tuple Number Number) (Tuple Number Number)

space :: Number
space = Math.pi / 2.0 / 4.0

points :: Array Number
points = map ((*) space) [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]

lines :: Array LineObj
lines  = map ptl points
  where
    ptl p = { start: p, end: p + Math.pi }

linePositions :: LineObj -> LineCoords
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

drawLines :: forall e. Array LineObj -> Context2D -> Eff (canvas :: CANVAS | e) Unit
drawLines lineobjs ctx = foreachE lines (\lo -> void $ drawLine lo ctx)

drawLine :: forall e. LineObj -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawLine lo ctx = do
  let coords = linePositions lo
  let begin  = fst coords
  let end    = snd coords

  beginPath ctx
  moveTo ctx (fst begin) (snd begin)
  lineTo ctx (fst end)   (snd end)
  stroke ctx

main :: forall e. (Partial) => Eff (canvas :: CANVAS | e) Unit
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  drawCircle ctx
  drawLines lines ctx

-- main = do
--   log $ show points
