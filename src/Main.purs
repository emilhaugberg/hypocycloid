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
import Data.Array (zip)
import Graphics.Canvas (CANVAS, Context2D, arc, beginPath, clearRect, fill, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, stroke)
import Math as Math

-- Experimental: Doesn't work in IE.
-- See https://developer.mozilla.org/en/docs/Web/API/CanvasRenderingContext2D/ellipse
foreign import ellipse :: forall e. Ellipse -> Context2D -> Eff (canvas :: CANVAS | e) Context2D

newtype Angle   = Angle Number
derive newtype instance semiringAngle :: Semiring Angle

type X = Number
type Y = Number
type Position = { x :: Number, y :: Number }
type Ellipse  =
  { x          :: Number
  , y          :: Number
  , rx         :: Number
  , ry         :: Number
  , rotation   :: Angle
  , startAngle :: Angle
  , endAngle   :: Angle
  }

width  = 600.0
height = 600.0

radius = 250.0
dotRadius = 10.0

centerX = width  / 2.0
centerY = height / 2.0

nums = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]

startingAngles = map (\_ -> Angle $ Math.pi / 10.0) nums

space :: Angle
space = Angle (Math.pi / 2.0 / 4.0)

ellipsAngles :: Array Angle
ellipsAngles = map ((*) space <<< Angle) nums

ellipses :: Array Ellipse
ellipses = map e ellipsAngles
  where
    e angle =
      { x         : centerX
      , y         : centerY
      , rx        : 0.0
      , ry        : radius
      , rotation  : angle
      , startAngle: Angle 0.0
      , endAngle  : Angle (Math.pi * 2.0)
      }

drawEllipse :: forall e. Ellipse -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawEllipse e ctx = do
  beginPath ctx
  ellipse e ctx
  stroke ctx

drawCircle :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawCircle ctx = do
  let a = { x: centerX, y: centerY, r: radius, start: 0.0, end: Math.pi * 2.0 }
  beginPath ctx
  arc ctx a
  setFillStyle "#F44336" ctx
  fill ctx

drawPoint :: forall e. Position -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawPoint pos ctx = do
  let a = { x: pos.x, y: pos.y, r: 10.0, start: 0.0, end: Math.pi * 2.0 }
  beginPath ctx
  arc ctx a
  setFillStyle "white" ctx
  fill ctx

fromAngle :: Angle -> Number
fromAngle (Angle x) = x

move :: Angle -> Angle
move (Angle ang) = Angle (ang + (Math.pi / 20.0))

rotate :: Angle -> Position -> Position
rotate ang pos = { x: nx, y: ny }
  where
    nx   = (cos * (pos.x - centerX)) + (sin * (pos.y - centerY)) + centerX
    ny   = (cos * (pos.y - centerY)) - (sin * (pos.x - centerX)) + centerY
    cos  = Math.cos ang'
    sin  = Math.sin ang'
    ang' = fromAngle ang

pointPos :: Angle -> Angle -> Position
pointPos ang angleR = rotate angleR { x: x, y: y }
  where
    x      = x' ang
    y      = y' ang
    y' ang = centerX + (radius * (Math.sin $ ang'))
    x' ang = centerY + (0.0    * (Math.cos $ ang'))
    ang'   = fromAngle ang

main :: forall e. (Partial) => Eff (ref :: REF, canvas :: CANVAS, timer :: TIMER | e) Unit
main = void $ do
  Just canvas <- getCanvasElementById "canvas"
  ctx         <- getContext2D canvas
  angles'     <- newRef $ ellipsAngles

  setInterval 150 $ void $ do
    clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}
    modifyRef angles' (map move)

    angles'' <- readRef angles'

    let xs = zip angles'' ellipsAngles

    drawCircle ctx
    foreachE ellipses \ellipse -> void $ drawEllipse ellipse ctx
    foreachE xs \angleTup -> void $ drawPoint (pointPos (fst angleTup) (snd angleTup)) ctx
