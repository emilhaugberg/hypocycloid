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

foreign import ellipse :: forall e. Ellipse -> Context2D -> Eff (canvas :: CANVAS | e) Context2D

type Line       = { start :: Angle, end :: Angle }
type Position   = Tuple Number Number
type LineCoords = Tuple Position Position

newtype Angle   = Angle Number
derive newtype instance semiringAngle :: Semiring Angle
derive newtype instance showgAngle :: Show Angle

type Ellipse =
  { x          :: Number
  , y          :: Number
  , radiusX    :: Number
  , radiusY    :: Number
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

space :: Angle
space = Angle (Math.pi / 2.0 / 4.0)

angles :: Array Angle
angles = map ((*) space <<< Angle) [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]

ellipses :: Array Ellipse
ellipses = map e angles
  where
    e angle =
      { x: centerX
      , y: centerY
      , radiusX: 0.0
      , radiusY: radius
      , rotation: angle
      , startAngle: Angle 0.0
      , endAngle: Angle (Math.pi * 2.0)
      }

drawEllipse :: forall e. Ellipse -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawEllipse e ctx = do
  beginPath ctx
  ellipse e ctx
  stroke ctx

drawCircle :: forall e. Context2D -> Eff (canvas :: CANVAS | e) Context2D
drawCircle ctx = do
  let a = { x: width / 2.0, y: height / 2.0, r: radius, start: 0.0, end: Math.pi * 2.0 }
  beginPath ctx
  arc ctx a
  setFillStyle "#F44336" ctx
  fill ctx

-- drawDot :: forall e. Angle -> Context2D -> Eff (canvas :: CANVAS | e) Context2D
-- drawDot (Angle ang) ctx = do
--   let dot = calculateDotPos (Angle ang)
--   let a = { x: fst dot, y: snd dot, r: dotRadius, start: 0.0, end: Math.pi * 2.0 }
--   beginPath ctx
--   arc ctx a
--   setFillStyle "white" ctx
--   fill ctx

-- calculateDotPos :: Angle -> Position
-- calculateDotPos (Angle ang) =
--   Tuple
--   (calculatePosition Math.cos radius)
--   (calculatePosition Math.sin 0.0)
--   where
--     calculatePosition f r = (f ang) * r + 300.0

-- move :: Angle -> Angle
-- move (Angle ang) = Angle (ang + (Math.pi / 10.0))

startingAngles :: Array Angle
startingAngles = map Angle [Math.pi / 2.0, Math.pi, Math.pi / 4.0, Math.pi / 5.0]

-- main :: forall e. (Partial) => Eff (ref :: REF, canvas :: CANVAS, timer :: TIMER | e) Unit
main = void $ do
  Just canvas <- getCanvasElementById "canvas"
  ctx         <- getContext2D canvas
  -- dots        <- newRef $ startingdots
  drawCircle ctx
  -- drawLines lines ctx
  foreachE ellipses \e -> void $ drawEllipse e ctx
  -- foreachE startingdots \d -> void $ drawDot d ctx

  -- log $ show startingdots
  -- setInterval 100 $ void $ do
  --   clearRect ctx {x: 0.0, y: 0.0, w: width, h: height}
  --   modifyRef dots (map move)
  --   dots' <- readRef dots
  --   drawCircle ctx
  --   drawLines lines ctx
  --   foreachE dots' \d -> void $ drawDot d ctx

-- startingdots :: Array Angle
-- startingdots = map (\l -> l.start) lines
