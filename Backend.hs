module Backend where
  
import TinySVG

{-
  Draw a graphic like test1.tsvg.
-}
drawGraphic :: String
drawGraphic = 
  let g = group [fillColor (NamedColor "blue") $ circle (pt (300, 300)) 100, fillColor 
          (NamedColor "blue") $ circle (pt (700, 300)) 100, noFill $ strokeColor 
          (NamedColor "black") $ path [moveto $ pt (500, 450), lineto $ pt (600, 600), 
          lineto $ pt (400, 600), lineto $ pt (500, 450)], noFill $ strokeColor 
          (NamedColor "black") $ path [moveto $ pt (300, 700), cbezier (pt (400, 800)) 
          (pt (600, 800)) (pt (700, 700)) ]]
  in toXML 1000 1000 g

{-
  Create a svg file from a string.
-}
createSVGFromString :: IO()
createSVGFromString = do
  writeFile "test1.svg" drawGraphic
  