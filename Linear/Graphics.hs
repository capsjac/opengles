-- |
module Linear.Graphics where
import Linear.V2
import Data.Word

-- sequence [($ cos (x/100)) | x <- [0..100]] (\x -> V3 0 x 0)
-- [V2 x $ sin (x/50) | x <- [0..100]]

circle2 :: (Ord a, Enum a, Floating a) => a -> [V2 a]
circle2 n | n > 1 = [V2 (cos t) (sin t) | t <- [0,2*pi/n..2*pi]]

rectangle2 :: Num a => a -> a -> a -> a -> [V2 a]
rectangle2 x y w h = [V2 x y, V2 (x+w) y, V2 (x+w) (y+h), V2 x (y+h)]

yEqual2 :: Enum a => (a -> a) -> a -> a -> [V2 a]
yEqual2 f from to = [V2 x (f x) | x <- [from..to]]

xEqual2 :: Enum a => (a -> a) -> a -> a -> [V2 a]
xEqual2 g from to = [V2 (g y) y | y <- [from..to]]

-- | 2D Rectangle covers whole screen.
simpleQuad2 :: Num a => [V2 a]
simpleQuad2 = [V2 (-1) 1, V2 1 1, V2 (-1) (-1), V2 1 (-1)]

-- | Rectangle covers whole texture.
wholeUV :: Num a => [V2 Word8]
wholeUV = [V2 0 0, V2 255 0, V2 0 255, V2 255 255]

-- | Build a matrix that modify 2D aspect raito.
scale2 :: Num a => a -> a -> V2 (V2 a)
scale2 xScale yScale = V2 (V2 xScale 0) (V2 0 yScale)

-- | 2D rotation matrix.
rotate2 :: Floating a => a -> V2 (V2 a)
rotate2 a = V2 (V2 (cos a) (sin a)) (V2 (-sin a) (cos a))

