{- |
A really tiny module for creating simple scalable vector graphics
and for exporting them into the SVG XML format.

(c) 2011-01-24 Dennis Walter <dennis.walter@dfki.de>
-}
module TinySVG (
-- * Graphics primitives
-- ** Data types
  Point(..),
  Graphics,
  ColorInfo (..),
-- ** Operations
  pt, pts, smult,
  setAttr, setAttrs, setStyleAttr, 
-- * Graphics elements
-- ** Basic shapes
  circle, rect, rectP, ellipse, line, polygon, polyline, group, 
-- ** Paths
  path, lineto, rlineto, moveto, rmoveto, cbezier, rcbezier, qbezier, rqbezier, 
  ellarc, rellarc, closepath,
-- *** Simpler combinators
  arc, slice,
-- ** Images
  image, imageStretched, 
-- ** Text
  text, textBold, textFont,  textSize,
-- * Transformations
  translate, rotateAt, rotate, scale, scaleXY, skewX, skewY, 
-- * Fill & stroke
  fillColor, noFill, strokeColor, strokeWidth,
-- * Output
  toXML,
) where

import Text.Printf (printf)
import Data.Char (toUpper)
-- import Data.Complex
import Data.List (intersperse, intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.XML.Light hiding (Attr)
import qualified Text.XML.Light as X (Attr(..))
import Prelude hiding (elem)
import Numeric (showHex)

-- | Attributes of graphics elements.
data Attr =
    Transf [(String, [Double])] -- ^ A transformation (@transform@ attribute)
  | Style (Map String String)   -- ^ The many @style@ settings (esp. @color@)
  | Other String                -- ^ Anything else

-- | Attributes are a mapping from key to 'Attr'
type Attrs = Map String Attr

-- | No alarms, no surprises: just points.
data Point = Pt { ptx :: Double, pty :: Double } deriving Eq

-- | The elementary /SVG/ shapes.
data GShape =
    GRect Point Double Double    -- ^ anchor + width + height
  | GCirc Point Double           -- ^ center + radius
  | GEllipse Point Double Double -- ^ center + xradius + yradius
  | GLine Point Point            -- ^ start + end
  | GPoly Bool [Point]           -- ^ closed? + points
  | GImage Point String Double Double  -- ^ anchor + href + width + height
  | GText Point String           -- ^ anchor + text
  | GPath [PathData]            -- ^ path instructions 
  | GGroup [Graphics]              -- ^ groups graphics

-- | A graphics is a shape together with some attributes
data Graphics = G { gShape :: GShape, gAttrs :: Attrs }

-- Colors.
data ColorInfo =
    RGB Int Int Int                -- ^ red green blue in @[0..255]@
  | NamedColor String              -- ^ Predefined SVG names
  | BW Double                      -- ^ black/white in @0.0@ to @1.0@.


data PathData =
    Dm Bool Point                   -- ^ moveto
  | Dl Bool Point                   -- ^ lineto
  | Dc Bool Point Point Point      -- ^ cubic bezier
  | Dq Bool Point Point            -- ^ quadratic bezier
  | Da Bool Double Double Double Bool Bool Point -- ^ elliptical arc
  | Dz                               -- ^ closepath
    -- Hh Vv Ss Tt are explicitly encoded via Dl Dc Dq

instance Show ColorInfo where
    showsPrec _ (RGB r g b) = showString (printf "#%02x%02x%02x" r g b)
    showsPrec _ (NamedColor name) = showString name
    showsPrec _ (BW x) = 
      let n = round (255 * x) :: Int in showString "#" . showHex n . showHex n . showHex n
      
instance Show Attr where
    show (Style m) = foldr (\ (k,v) r-> k ++ ":" ++ v ++ ";" ++ r) "" (Map.toList m)
    show (Transf ops) = intercalate " " $ map (\ (f, args)-> f ++ "(" ++ intercalate " " (map show args) ++ ")") ops
    show (Other s) = s


-- Internal grouping shorthand.
gr :: GShape -> Graphics
gr shape = G {gShape = shape, gAttrs = defaultAttrs}

-- | Convenience function: create a 'Point' from given coordinates.
pt :: (Double, Double) -> Point
pt (x, y) = Pt x y

instance Num Point where
  Pt x1 y1 + Pt x2 y2 = Pt (x1+ x2) (y1+ y2)
  Pt x1 y1 * Pt x2 y2 = undefined -- Pt (x1* x2) (y1* y2)
  abs (Pt x y)        = Pt (abs x) (abs y)
  signum (Pt x y)     = Pt (signum x) (signum y)
  negate (Pt x y)     = Pt (-x) (-y)
  fromInteger i       = Pt (fromInteger i) 0

instance Show Point where
  show (Pt x y) = "("++ show x++ ", "++ show y++ ")"  

-- | Scalar multiplication
smult :: Double-> Point-> Point
smult r (Pt x y) = Pt (r* x) (r* y)

-- | Construction from polar form
mkPolar :: Double-> Double-> Point
mkPolar r phi = Pt (r* cos phi) (r* sin phi)


-- | Convenience function, equivalent to @map pt@.
pts :: [(Double, Double)] -> [Point]
pts = map pt

defaultAttrs :: Attrs
defaultAttrs = Map.empty


{- | Set a graphics attribute. Not to be used for /special/ attributes
like color or stroke width (use 'setStyleAttr' instead).
-}
setAttr :: String -> String -> Graphics -> Graphics
setAttr key val g = g { gAttrs = Map.insert key (Other val) (gAttrs g) }

setAttrs :: [(String, String)] -> Graphics -> Graphics
setAttrs attrs g = foldr (uncurry setAttr) g attrs

-- | Set attributes that appear within SVG's @style@ attribute.
setStyleAttr :: String -> String -> Graphics -> Graphics
setStyleAttr key val g =
    let style = "style" in
    case Map.lookup style (gAttrs g) of
      Nothing -> g { gAttrs = Map.insert style (Style $ Map.fromList [(key, val)]) (gAttrs g) }
      Just (Style m) -> g { gAttrs = Map.insert style (Style $ Map.insert key val m) (gAttrs g) }
      _ -> g

-- * Basic shapes

-- | A circle, defined by center point and radius.
circle :: Point -> Double -> Graphics
circle p radius = gr (GCirc p radius)

-- | A rectangle defined by lower left edge point, width, and height.
rect :: Point -> Double -> Double -> Graphics
rect p width height = gr (GRect p width height)

-- | A rectangle defined by lower left and upper right edge points.
rectP :: Point -> Point -> Graphics
rectP p@(Pt x1 y1) (Pt x2 y2) = rect p (x2 - x1) (y2 - y1)

-- | An ellipse defined by center point and x/y radii.
ellipse :: Point -> Double -> Double -> Graphics
ellipse p xRadius yRadius = gr (GEllipse p xRadius yRadius)

{- | A line, drawn with /black stroke/ by default. (Useful, because
lines without strokes are invisible.)
-}
line :: Point -> Point -> Graphics
line p1 p2 = setStyleAttr "stroke" "black" $
             gr (GLine p1 p2)


{- | A polygon. The first point is implicitly taken as the final point
and need not be contained in the list as the last one. -}
polygon :: [Point] -> Graphics
polygon ps = gr (GPoly True ps)

-- | A polygonal line.
polyline :: [Point] -> Graphics
polyline ps = gr (GPoly False ps)


-- ** General Paths

-- | The most general of all geometric shapes: the /path/.
path :: [PathData] -> Graphics
path instr = gr (GPath instr)

-- | Draw a line from the /current point/ to @p@, given in absolute coordinates.
lineto :: Point -> PathData
lineto p = Dl False p

-- | Draw a line from the /current point/ to @p@, given in relative coordinates.
rlineto :: Point -> PathData
rlineto p = Dl True p

-- | Move the /current point/ to @p@, given in absolute coordinates.
moveto :: Point -> PathData
moveto p = Dm False p

-- | Move the /current point/ to @p@, given in relative coordinates.
rmoveto :: Point -> PathData
rmoveto p = Dm True p


{- | @cbezier cp1 cp2 p@ draws a cubic bezier curve from the /current point/
to @p@ using the control points @cp1@ and @cp2@.
-}
cbezier :: Point -> Point -> Point -> PathData
cbezier cp1 cp2 p = Dc False cp1 cp2 p

{- | Like 'cbezier', but based on relative coordinates. 
-}
rcbezier :: Point -> Point -> Point -> PathData
rcbezier cp1 cp2 p = Dc True cp1 cp2 p

{- | @qbezier cp p@ draws a quadratic bezier curve from the /current point/
to @p@ using the control point @cp@.
-}
qbezier :: Point -> Point -> PathData
qbezier cp p = Dq False cp p
{- | Like 'qbezier', but based on relative coordinates. 
-}
rqbezier :: Point -> Point -> PathData
rqbezier cp p = Dq True cp p

-- | Closes the current path by returning to the starting point.
closepath :: PathData
closepath = Dz

{- | Draw an elliptical arc in the curious way that SVG suggests.
   @ellarc rx ry xAxisRotation largeArcFlag sweepFlag (Pt x y)@ draws an
   elliptical arc from the current position to @(x, y)@. @rx@ and @ry@
   define the radii of the ellipse, which is rotated relative to the
   current coordinate system by @xAxisRotation@. (See the SVG standard
   for a description of @large-arc-flag@ and @sweep-flag@.
 -} 
ellarc :: Double -> Double -> Double -> Bool -> Bool -> Point -> PathData 
ellarc rx ry xar laf sf p = Da False rx ry xar laf sf p

-- | Draw an elliptical arc using relative coordinates (cf. 'ellarc').
rellarc :: Double -> Double -> Double -> Bool -> Bool -> Point -> PathData
rellarc rx ry xar laf sf p = Da True rx ry xar laf sf p


{- | @arc c r a b@ draws a circular arc with center @c@ and radius @r@,
starting at angle @a@ and ending at @b@ (given in /degrees/, as usual).
For full arcs (from 0 to 360) you'd rather use 'circle'.
-}
arc :: Point -> Double -> Double -> Double -> Graphics
arc c r a b = 
  let phi = a / 180 * pi
      psi = b / 180 * pi
      p0 = c + mkPolar r phi
      p1 = c + mkPolar r psi in
  path [moveto p0, ellarc r r 0 (mod360 (b - a) >= 180) True p1]

{- | @slice c r a b@ draws a slice (like an arc, but filled) with center @c@
and radius @r@, starting at angle @a@ and ending at @b@ (given in
/degrees/, as usual).  
-}
slice :: Point -> Double -> Double -> Double -> Graphics
slice c r a b = 
  let phi = a / 180 * pi
      psi = b / 180 * pi
      p0 = c + mkPolar r phi
      p1 = c + mkPolar r psi in
  path [moveto c, lineto p0, ellarc r r 0 (mod360 (b - a) >= 180) True p1, lineto c]

mod360 :: Double -> Double
mod360 x
  | x < 0 = _up (x+360)
  | x >= 360 = _down (x-360)
  | otherwise = x
  where _up x = if x >= 0 then x else _up (x+360)
        _down x = if x < 360 then x else _down (x-360)

-- * Grouping

-- | Group a list of graphics together.
group :: [Graphics] -> Graphics
group = gr . GGroup


-- * Images

-- | Create a graphics from an external image, e.g. a /PNG/ file. 
image :: Point -> String -> Double -> Double -> Graphics
image href p width height = gr (GImage href p width height)

{- | Include an image that occupies all allocated space, possibly by stretching it. -}
imageStretched :: Point -> String -> Double -> Double -> Graphics
imageStretched href p width height = 
  setAttr "preserveAspectRatio" "none" $
  gr (GImage href p width height)


-- * Text

{- | Write some (unformatted) text at the given position. -}
text :: Point -> String -> Graphics
text p t = gr $ GText p t

-- | Set the @font-weight@ to @bold@.
textBold :: Graphics -> Graphics
textBold = setStyleAttr "font-weight" "bold"

-- | Set the @font-family@.
textFont :: String -> Graphics -> Graphics
textFont = setStyleAttr "font-family"

textSize :: Double -> Graphics -> Graphics
textSize size = setStyleAttr "font-size" (showCSS size)


-- * Transformations


-- Prepends a transformation onto the given transformations of the 'Graphics'.
updateTransform :: (String, [Double]) -> Graphics -> Graphics
updateTransform fArgs g =
  case Map.lookup "transform" (gAttrs g) of
    Just (Transf ops) -> g { gAttrs = Map.insert "transform" (Transf $ fArgs : ops) (gAttrs g) }
    Nothing -> g { gAttrs = Map.insert "transform" (Transf [fArgs]) (gAttrs g) }
    _ -> g -- Erroneous attribute for 'transform'

-- | Translation (@translate(x y)@).
translate :: Point -> Graphics -> Graphics
translate (Pt x y) = updateTransform ("translate", [x, y])

{- | Rotation around a given point (@rotate(phi x y)@).
As in SVG, the angle must be given in /degrees/, not radians.
Positive values yield a /clockwise/ rotation (in contrast to
common mathematical practise).
-}
rotateAt :: Point -> Double -> Graphics -> Graphics
rotateAt (Pt x y) phi = updateTransform ("rotate", [phi, x, y])

-- | Rotation around the origin.
rotate :: Double -> Graphics -> Graphics
rotate = rotateAt (Pt 0 0)

-- | Uniform scaling (@scale(s s)@).
scale :: Double -> Graphics -> Graphics
scale s = updateTransform ("scale", [s, s])

-- | Non-uniform scaling (@scale(x y)@).
scaleXY :: Double -> Double -> Graphics -> Graphics
scaleXY xScale yScale = updateTransform ("scale", [xScale, yScale])

-- | Skewing along the x-axis (@skewX(s)@).
skewX :: Double -> Graphics -> Graphics
skewX s = updateTransform ("skewX", [s])

-- | Skewing along the y-axis (@skewY(s)@).
skewY :: Double -> Graphics -> Graphics
skewY s = updateTransform ("skewY", [s])

{- | Fill a graphics with color.
(Sets the @fill@ style attribute of the graphics to the given color.)
-}
fillColor :: ColorInfo -> Graphics -> Graphics
fillColor cInf g = setStyleAttr "fill" (show cInf) g

-- | Sets the @fill@ attribute to @none@.
noFill :: Graphics -> Graphics
noFill g = setStyleAttr "fill" "none" g

{- | Sets the @stroke@ to some color. Try this, if your graphics objects
won't show up.
-}
strokeColor :: ColorInfo -> Graphics -> Graphics
strokeColor cInf g = setStyleAttr "stroke" (show cInf) g

-- | Adjust the @stroke-width@.
strokeWidth :: Double -> Graphics -> Graphics
strokeWidth w g = setStyleAttr "stroke-width" (showCSS w) g

class ShowCSS a where
  showCSS :: a -> String

instance ShowCSS Double where
  showCSS d = printf "%f" d

-- * Output

svgVersion :: String
svgVersion = "1.1"

-- | Draw a 'Graphics' inside the given bounding box
toXML :: Double -> Double -> Graphics -> String
toXML width height g =
    let topElement =
          Element {
             elName = svgName "svg",
             elAttribs = [attr "xmlns:svg" "http://www.w3.org/2000/svg"
                         ,attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
                         ,attr "version" svgVersion
                         ,attrD "width" width
                         ,attrD "height" height
                         ],
             elContent = [Elem (toXML' g)],
             elLine = Nothing } in
    ppTopElement topElement

-- | Generates a point sequence string as required by /SVG/'s @<polygon>@ and friends.
pointSeq :: [Point] -> String
pointSeq = concat . intersperse ", " . map (\ (Pt x y)-> show x ++ " " ++ show y)

-- | Generates a path in SVG's compact syntax
pathSeq :: [PathData] -> String
pathSeq = f
  where 
    char b c = if b then c else toUpper c
    f pd = case pd of
      [] -> ""
      (Dm b (Pt x y)):z -> char b 'm' : show x ++ " " ++ show y ++ f z
      (Dl b (Pt x y)):z -> char b 'l' : show x ++ " " ++ show y ++ f z
      (Dc b (Pt x1 y1) (Pt x2 y2) (Pt x y)):z -> 
        (char b 'c' : show x1 ++ " " ++ show y1 ++ " " ++ 
         show x2 ++ " " ++ show y2 ++ " " ++
         show x ++ " " ++ show y ++ f z)
      (Dq b (Pt x1 y1) (Pt x y)):z -> 
        (char b 'c' : show x1 ++ " " ++ show y1 ++ " " ++
         show x ++ " " ++ show y ++ f z)
      (Da b rx ry xar laf sf (Pt x y)):z ->
        (char b 'a' : show rx ++ " " ++ show ry ++ " " ++ show xar ++ " " ++ 
         (if laf then "1 " else "0 ") ++ (if sf then "1 " else "0 ") ++
         show x ++ " " ++ show y)
      Dz:z -> "z" ++ f z
  
  
toXML' :: Graphics -> Element
toXML' g =
    case gShape g of
        GRect (Pt x y) width height ->
          leafElem "rect" ([attrD "x" x, attrD "y" y,
                            attrD "width" width, attrD "height" height] ++
                            shapeAttrs)
        GCirc (Pt x y) radius ->
          leafElem "circle" ([attrD "cx" x, attrD "cy" y, attrD "r" radius] ++ shapeAttrs)
        GEllipse (Pt x y) xRadius yRadius ->
          leafElem "circle" ([attrD "cx" x, attrD "cy" y,
                              attrD "rx" xRadius, attrD "ry" yRadius] ++ shapeAttrs)
        GLine (Pt x1 y1) (Pt x2 y2) ->
          leafElem "line" ([attrD "x1" x1, attrD "y1" y1, attrD "x2" x2, attrD "y2" y2] ++ shapeAttrs)
        GPoly True ps ->
          leafElem "polygon" ([attr "points" (pointSeq ps)] ++ shapeAttrs)
        GPoly False ps ->
          leafElem "polyline" ([attr "points" (pointSeq ps)] ++ shapeAttrs)
        GText (Pt x y) text ->
          textElem "text" ([attrD "x" x, attrD "y" y] ++ shapeAttrs) text
        GImage (Pt x y) href width height ->
          leafElem "image" ([attr "xlink:href" href, attrD "x" x, attrD "y" y, 
                             attrD "width" width, attrD "height" height] ++ shapeAttrs)
        GPath pd -> 
          leafElem "path" ([attr "d" (pathSeq pd)] ++ shapeAttrs)
        GGroup gs ->
          elem "g" shapeAttrs (map toXML' gs)
  where shapeAttrs = Map.foldWithKey (\ k v r-> attr k (show v) : r) [] (gAttrs g)



-- XML utilities

--  Create an attribute with unqualified key.
attr :: String -> String -> X.Attr
--  Create a numeric (unqual.) attribute.
attrD :: String -> Double -> X.Attr
--  Create an @svg@ element with given name, attributes and children.
elem :: String -> [X.Attr] -> [Element] -> Element
--  Create a childless @svg@ element.
leafElem :: String -> [X.Attr] -> Element
-- Create text content.
cdata :: String -> CData
-- Create an element with text content, properly escaped only at output time.
textElem :: String -> [X.Attr] -> String -> Element

svgName :: String -> QName
svgName s = (unqual s) {qPrefix = Just "svg"}

attr key val = X.Attr { attrKey = unqual key, attrVal = val }
attrD key val = attr key (show val)

elem name attrs children =
  Element { elName = svgName name, elAttribs = attrs, elContent = map Elem children, elLine = Nothing }

leafElem name attrs = elem name attrs []

cdata text = CData { cdVerbatim = CDataText, cdData = text, cdLine = Nothing }

textElem name attrs text = 
  Element { elName = svgName name, elAttribs = attrs, elContent = [Text $ cdata text], elLine = Nothing }


{-
Notes on SVG:

1) inkscape uses <path>s with concrete coordinates
as connectors between objects. It marks them as connectors
by special attributes from the inkscape: namespace. I assume that 
the coordinates are computed by inkscape in a similar way as I 
plan to do this (compute the bounding box & anchor the path to it)


-}

