module Linear.Graphics where
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix

ortho :: Fractional a
	=> (a, a) -- ^ (left, right)
	-> (a, a) -- ^ (bottom, top)
	-> (a, a) -- ^ (near, far)
	-> M44 a
ortho (l,r) (b,t) (n,f) =
	V4 (V4 (2/(r-l)) 0 0 0)
	   (V4 0 (2/(t-b)) 0 0)
	   (V4 0 0 (-2/(f-n)) 0)
	   (V4 (-(r+l)/(r-l)) (-(t+b)/(t-b)) (-(f+n)/(f-n)) 1)

-- | 'ortho' with a different parametrization.
ortho' :: Fractional a
	=> V3 a -- ^ (left, top, near)
	-> V3 a -- ^ (right, bottom, far)
	-> M44 a
ortho' (V3 l t n) (V3 r b f) = ortho (l,r) (b,t) (n,f)

-- | \"Perspective projecton\" matrix
frustum :: Fractional a
	=> (a, a) -- ^ (left, right)
	-> (a, a) -- ^ (bottom, top)
	-> (a, a) -- ^ (near, far)
	-> M44 a
frustum (l,r) (b,t) (n,f) =
	V4 (V4 (2*n/(r-l)) 0 0 0)
	   (V4 0 (2*n/(t-b)) 0 0)
	   (V4 ((r+l)/(r-l)) ((t+b)/(t-b)) (-(f+n)/(f-n)) (-1))
	   (V4 0 0 (-2*f*n/(f-n)) 0)

-- | 'frustum' with a different parametrization.
frustum' :: Fractional a
	=> V3 a -- ^ (left, top, near)
	-> V3 a -- ^ (right, bottom, far)
	-> M44 a
frustum' (V3 l t n) (V3 r b f) = frustum (l,r) (b,t) (n,f)

-- | Inverse of 'frustum'
frustumInv :: Fractional a
	=> (a, a) -- ^ (left, right)
	-> (a, a) -- ^ (bottom, top)
	-> (a, a) -- ^ (near, far)
	-> M44 a
frustumInv (l,r) (b,t) (n,f) =
	V4 (V4 (0.5*(r-l)/n) 0 0 0)
	   (V4 0 (0.5*(t-b)/n) 0 0)
	   (V4 0 0 0 (0.5*(n-f)/(f*n)))
	   (V4 (0.5*(r+l)/n) (0.5*(t+b)/n) (-1) (0.5*(f+n)/(f*n)))

-- sequence [($ cos (x/100)) | x <- [0..100]] (\x -> V3 0 x 0)
-- [V2 x $ sin (x/50) | x <- [0..100]]

circle2d :: (Ord a, Enum a, Floating a) => a -> [V2 a]
circle2d n | n > 1 = [V2 (cos t) (sin t) | t <- [0,2*pi/n..2*pi]]

rectangle :: Num a => a -> a -> a -> a -> [V2 a]
rectangle x y w h = [V2 x y, V2 (x+w) y, V2 (x+w) (y+h), V2 x (y+h)]

yEqual f from to = [V2 x (f x) | x <- [from..to]]

xEqual g from to = [V2 (g y) y | y <- [from..to]]

-- | Billboard helper
simpleQuad = [V2 (-1) 1, V2 1 1, V2 (-1) (-1), V2 1 (-1)]

-- | Whole texture
uvWhole = [V2 0 0, V2 1 0, V2 0 1, V2 1 1]

-- | Modify 2D aspect raito
scale2 xScale yScale = V2 (V2 xScale 0) (V2 0 yScale)

-- | 2D rotation matrix
rotate2 a = V2 (V2 (cos a) (sin a)) (V2 (-sin a) (cos a))

