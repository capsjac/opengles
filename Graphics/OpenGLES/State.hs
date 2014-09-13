-- Draw configurations on Rasterization and Per-Fragment Operations.
-- Note: Graphic state is sticky.
module Graphics.OpenGLES.State where
import Data.Int
import Data.Word
import Graphics.OpenGLES.Base

-- * Graphics State

-- | Float value clamped to [0,1]
type Clampf = Float

begin :: Capability -> GL ()
begin (Capability cap) = glEnable cap

end :: Capability -> GL ()
end (Capability cap) = glDisable cap

newtype Capability = Capability GLenum deriving Showculling = Capability 0x0B44blend = Capability 0x0BE2dither = Capability 0x0BD0stencilTest = Capability 0x0B90depthTest = Capability 0x0B71scissorTest = Capability 0x0C11polygonOffsetFill = Capability 0x8037sampleAlphaToCoverage = Capability 0x809EmultisampleCoverage = Capability 0x80A0primitiveRestartFixedIndex = Capability 0x8D69rasterizerDiscard = Capability 0x8C89

lineWidth :: Float -> GL ()
lineWidth = glLineWidth

-- | True if clockwise
frontFace :: Bool -> GL ()
frontFace cw = glFrontFace (if cw then 0x900 else 0x901)

cullFace :: CullFace -> GL ()
cullFace (Culling x) = glCullFace x

newtype CullFace = Culling GLenum deriving Show
hideFront = Culling 0x0404
hideBack = Culling 0x0405
hidePolygons = Culling 0x408

polygonOffset
	:: Float -- ^ factor
	-> Float -- ^ units
	-> GL ()
polygonOffset factor units = glPolygonOffset factor units

scissor
	:: Int32 -- ^ left
	-> Int32 -- ^ bottom
	-> Int32 -- ^ width
	-> Int32 -- ^ height
	-> GL ()
scissor l b w h = glScissor l b w h

sampleCoverage
	:: Float -- ^ multisample coverage value [0,1]
	-> Bool -- ^ whether coverage masks should be inverted or not
	-> GL ()
sampleCoverage value invert =
	glSampleCoverage value (if invert then 1 else 0)

stencilFunc :: CompFunc -> Int32 -> Word32 -> GL ()
stencilFunc (CompFunc func) comp mask =
	glStencilFunc func comp mask

stencilFuncSeparate :: CullFace -> CompFunc -> Int32 -> Word32 -> GL ()
stencilFuncSeparate (Culling cull) (CompFunc func) comp mask =
	glStencilFuncSeparate cull func comp mask

stencilOp :: StencilOp -> StencilOp -> StencilOp -> GL ()
stencilOp (StencilOp sfail) (StencilOp dpfail) (StencilOp dppass) =
	glStencilOp sfail dpfail dppass

stencilOpSeparate :: CullFace -> StencilOp -> StencilOp -> StencilOp -> GL ()
stencilOpSeparate (Culling cull) (StencilOp sfail)
		(StencilOp dpfail) (StencilOp dppass) =
	glStencilOpSeparate cull sfail dpfail dppass

depthFunc :: CompFunc -> GL ()
depthFunc (CompFunc func) = glDepthFunc func

newtype CompFunc = CompFunc GLenum deriving Show
glNever    = CompFunc 0x0200
glLess     = CompFunc 0x0201
glEqual    = CompFunc 0x0202
glLessEq   = CompFunc 0x0203
glGreater  = CompFunc 0x0204
glNotEq    = CompFunc 0x0205
glGreatEq  = CompFunc 0x0206
glAlways   = CompFunc 0x0207

newtype StencilOp = StencilOp GLenum deriving Show
opZero     = StencilOp 0x0000
opKeep     = StencilOp 0x1E00
opReplace  = StencilOp 0x1E01
opIncr      = StencilOp 0x1E02
opDecr     = StencilOp 0x1E03
opInvert   = StencilOp 0x150A
opIncrWrap = StencilOp 0x8507
opDecrWrap = StencilOp 0x8508

blendEquation :: BlendOp -> GL ()
blendEquation (BlendOp mode) = glBlendEquation mode

blendEquationSeparate :: BlendOp -> BlendOp -> GL ()
blendEquationSeparate (BlendOp rgb) (BlendOp a) =
	glBlendEquationSeparate rgb a

newtype BlendOp = BlendOp GLenum deriving Show
addBlending        = BlendOp 0x8006
subBlending        = BlendOp 0x800A
reverseSubBlending = BlendOp 0x800B

blendFunc :: BlendingFactor -> BlendingFactor -> GL ()
blendFunc (BlendingFactor src) (BlendingFactor dest) =
	glBlendFunc src dest

blendFuncSeparate :: BlendingFactor -> BlendingFactor -> BlendingFactor -> BlendingFactor -> GL ()
blendFuncSeparate (BlendingFactor srgb) (BlendingFactor drgb)
		(BlendingFactor salpha) (BlendingFactor dalpha) =
	glBlendFuncSeparate srgb drgb salpha dalpha

newtype BlendingFactor = BlendingFactor GLenum deriving Show
factorZero         = BlendingFactor 0
factorOne          = BlendingFactor 1
srcColor           = BlendingFactor 0x300
oneMinusSrcColor   = BlendingFactor 0x301
srcAlpha           = BlendingFactor 0x302
oneMinusSrcAlpha   = BlendingFactor 0x303
dstAlpha           = BlendingFactor 0x304
oneMinusDstAlpha   = BlendingFactor 0x305
dstColor           = BlendingFactor 0x306
oneMinusDstColor   = BlendingFactor 0x307
srcAlphaSaturate   = BlendingFactor 0x308
constColor         = BlendingFactor 0x8001
oneMinusConstColor = BlendingFactor 0x8002
constAlpha         = BlendingFactor 0x8003
oneMinusConstAlpha = BlendingFactor 0x8004

blendColor :: Clampf -> Clampf -> Clampf -> Clampf -> GL ()
blendColor r g b a = glBlendColor r g b a

generateMipmapHint :: Hint -> GL ()
generateMipmapHint (Hint hint) = glHint 0x8192 hint

fragmentShaderDerivativeHint :: Hint -> GL ()
fragmentShaderDerivativeHint (Hint hint) = glHint 0x8B8B hint

newtype Hint = Hint GLenum deriving Show
dontCare = Hint 0x1100
fastest  = Hint 0x1101
nicest   = Hint 0x1102
