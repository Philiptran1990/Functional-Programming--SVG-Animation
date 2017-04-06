--Author Philip Tran
module MyAnimation where

import Animation

maxWidth :: Length
maxWidth = 800

maxHeight :: Length
maxHeight = 600

--
picture :: Animation
picture =
 (animationSpinner 
 `plus`
  rotatingEllipses 20
 `plus`
 backGround (maxWidth/2)(maxHeight/2)(170)
 `plus`
 expandingRing
 )
 
--Forms the background layer of circles using recursion
backGround :: Length -> Length -> Length -> Animation
backGround x y z 
 | z > 8 = translate (always (x, y)) 
    (withBorder (always black) (always 1)
    (withoutPaint (circle (always (z))))) 
    `plus` 
    backGround (x+z/2)(y)(z/2) 
    `plus` 
    backGround (x-z/2)(y)(z/2) 
    `plus` 
    backGround (x)(y+z/2)(z/2) 
    `plus` 
    backGround (x)(y-z/2)(z/2) 
 | otherwise = translate (always (x, y)) 
    (withBorder (always black) (always 1)
    (withoutPaint (circle (always z))))
 
basicRings :: Colour -> Length -> Length -> Animation
basicRings c x y =
 withBorder (always c) (always x)
            (withoutPaint (circle (always y)))
 
--Recursively calls for a decreasingly sized elipses which rotate around the same axis
rotatingEllipses :: Length -> Animation
rotatingEllipses x 
 | x > 2 = translate (always (maxWidth/2, maxHeight/2))
    (rotate (spinner (1*x))(scale (always (x, 1)) 
    (withBorder (always black) (always 1)
    (withoutPaint (circle (always (1+x))))))) 
    `plus` rotatingEllipses (x-1)
 | otherwise = translate (always (maxWidth/2, maxHeight/2))(rotate (spinner (1*x))(scale (always (x, 1)) (withBorder (always black) (always 1)(withoutPaint (circle (always 5)))))) 
 
--Repeatedly creates rectangles around an axis forming a circular pattern of rectangles
outerRing :: Animation
outerRing = 
 (combine [rotate (always (i*2.5)) 
 (translate (always ((maxWidth/4),(0))) 
 (withPaint (always black) 
 (rect (always 75) (always 2))))
 | i <- [1..144]])

--Used to rotate all of the different sections of the animation. Moves various animations into place and then spins them around the new axis location
animationSpinner :: Animation
animationSpinner =
 translate (always ((maxWidth/2),(maxHeight/2))) 
 (combine [(rotate (spinner (60)) outerRing)
 `plus`
 basicRings black 3 220
 `plus` 
 (rotate (spinner (-50)) ellipseRing)
 `plus`
 basicRings white 10 188
 `plus`
 basicRings black 5 130
 ])

--Repeatedly creates ellipses in a circular pattern
ellipseRing :: Animation
ellipseRing = 
 (combine [rotate (always (i*6)) 
 (translate (always ((maxWidth/5),(0))) 
 (withBorder (always black) (always 3) 
 (withoutPaint (ellipse (always 30) (always 20)))))
 | i <- [1..60]])

--Creates increasingly smaller rings which are also increasingly transparent. 
--Uses cycleSmooth to make the rings compress and expand
expandingRing :: Animation
expandingRing =
 combine [translate (always (maxWidth/2, maxHeight/2))
 (withGenBorder (always black) (always (500)) (always 2)
 (withoutPaint (circle (cycleSmooth (4) [(420-i), (320-(i*2))]))))
 | i <- [1..20]]
 
test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture )