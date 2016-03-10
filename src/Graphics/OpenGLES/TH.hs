{-# LANGUAGE TemplateHaskell #-}
module Graphics.OpenGLES.TH (
  
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

data GLSLVersion = SLES1 | SLES3

glslHeader :: GLSLVersion -> DataType -> Q [Dec]


idiom生成
data宣言
シェーダ生成
コンパイル
attrib/uniform取得
バッファ生成
VAO生成

data TextShader = TextShader {
	_uColor :: Uniform TextShader Vec4
}

class UColor a where
	type UColor :: a -> b
	uColor :: a -> Uniform a b

instance UColor TextShader where
	type UColor TextShader = Vec4
	uColor = _uColor

glDraws [ resetcfg, begin blend
	 , srcColors*srcAlpha + dstColors*oneMinusSrcAlpha
	 , blender $ srcAlpha + (1-srcAlpha)
	 , srcAlpha `addBlending` oneMinusSrcAlpha
	 ] $ do
	program textShader
	uColor $= pure 1
	enable depthTest
	texSlot 0 tex
	draw triangleStrip 

--module Graphics.OpenGLES.FunctionalPipeline

--draw data = make drawcalls for it
-- Write a Haskell, Get Shaders for.
[glsl10|
Program TextShader
	mvpMatrix :: Mat4
	color :: Vec4
	uv :: Vec3
	pos :: Vec3
	texUnit :: Texture

	vColor = color
	gl_Position = mvpMatrix * vec4 pos 1.0
	gl_PointSize = float gl_VertexID
	gl_FragDepth = gl_FragDepth
	gl_FragColor = vColor * x (texture2D texUnit uv)

	Feedback pos uv color
|]

glsl10 [d|
data TextShader
	mvpMatrix :: Mat4
	color :: Vec4
	uv :: Vec3
	pos :: Vec3
	texUnit :: Texture

	vColor = color
	gl_Position = mvpMatrix * vec4 pos 1.0
	gl_PointSize = float gl_VertexID
	gl_FragDepth = gl_FragDepth
	gl_FragColor = vColor * x (texture2D texUnit uv)
	feedback pos uv color = FeedbackToThem
]


generates:
1)
putStrLn "hello, TH World!"
AppE (VarE 'putStrLn) (LitE (StringL "hello, TH World!"))

例2)
fact 0 = 1
fact n = n * fact (n - 1)
[FunD (mkName "fact")
  [Clause [LitP (IntegerL 0)] (NormalB (LitE (IntegerL 1))) []
  ,Clause [VarP $ mkName "n"] (NormalB (InfixE (Just (VarE $ mkName "n")) (VarE '(*)) (Just (AppE (VarE $ mkName "fact") (InfixE (Just (VarE $ mkName "n")) (VarE '(-)) (Just (LitE (IntegerL 1)))))))) []
]]

例3)
data MyGreatData = MGD String Int
[DataD [] (mkName "TextShader") [] 
 [NormalC (mkName "TextShader")
 	[ (IsStrict, AppT ConT ''Program)
 	, (IsStrict, ConT ''Int)
 	]
 ]
 [] ]


data TextShader (Program TextShader) (Uniform TS Mat4)...
instance UmvpMatrix where
  mvpMatrix (TextShader ..nth..) = nth
compileTextShader = do
	p <- glCompile (Feedback ["a","b","c"])
		[ vertexShader "text.vs" ..., ...]
		(...)
	TextShader p <$> uniform "mvpMatrix" <*> attrib "pos"

mkTextShader = do
	ts <- compileTextShader
	p <- glLoad app2gl textPos
	u <- glLoad app2gl textUVs
	a <- glVA [ pos ts &= posBuf, uv ts &= uvBuf ]
	return (ts, p, u, a)

drawText (ts, posBuf, uvBuf, vao) = do
	tex <- texString "The quick brown fox jumps over the lazy dog." 120
	setSampler tex (Sampler (clampToEdge,clampToEdge,Nothing) 16.0 (magLinear,minLinear))
	-- 効率的API
	glDraws textShader $ do
		setPipeline textShader
		setVertexArrays vao
		setRenderConfigs
			[ texSlot 0 tex
			, begin blend
			, addBlend srcAlpha oneMinusSrcAlpha ]
		setUniforms
			[ mvpMatrix $= mat
			, color $= V4 0.8 0.4 0.2 1 ]
		glDrawCall triangleStrip (takeFrom 0 4)
	-- 教育的API
	glDraw triangleStrip textShader
		[ texSlot 0 tex
		, begin blend
		, blendEquation addBlending
		, blendFunc srcAlpha oneMinusSrcAlpha ]
		[ mvpMatrix $= mat
		, color $= V4 0.8 0.4 0.2 1 ]
		vao (takeFrom 0 4)

-- shorthands
addBlend src dest = do
	blendEquation addBlending
	blendFunc src dest

subBlend src dest = do
	blendEquation subBlending
	blendFunc src dest

blendSub src dest = do
	blendEquation subBlendingReversed
	blendFunc src dest

srcAlpha `addBlend` oneMinusSrcAlpha
srcColor `subBlend` oneMinusSrcAlpha
srcAlpha `blendSub` oneMinusSrcAlpha
