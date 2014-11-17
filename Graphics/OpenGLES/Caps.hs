-- | Retriving implementation limits.
-- Do NOT call below APIs out of OpenGL threads.
-- (glGetString may return null pointer!)
module Graphics.OpenGLES.Caps where
import Foreign
import Foreign.C.String
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Internal (Shader(..))
import System.IO.Unsafe (unsafePerformIO)


-- * Extension detection

{-# NOINLINE glExtensions #-}
-- | The list of available extensions on this GL implementation.
glExtensions :: [String]
glExtensions = unsafePerformIO $
	glGetString 0x1F03 >>= peekCString >>= return.words

{-# NOINLINE hasExt #-}
-- | True if this GL has the extension.
hasExt :: String -> Bool
hasExt ext = ext `elem` glExtensions
--hasExt :: CString -> Bool
--glGetString 0x1F03 >>= indexOf ext /= -1

{-# NOINLINE hasES3 #-}
-- | True if this GL version is 3 or later. 
hasES3 :: Bool
hasES3 = glCap majorVersion > 2

-- | Internally used.
extVAO :: Maybe (GLsizei -> Ptr GLuint -> GL (),
	GLuint -> GL (),
	GLsizei -> Ptr GLuint -> GL ())
extVAO
	| hasES3 =
		Just (glGenVertexArrays, glBindVertexArray, glDeleteVertexArrays)
	| hasExt "GL_OES_vertex_array_object" =
		Just (glGenVertexArraysOES, glBindVertexArrayOES, glDeleteVertexArraysOES)
	| otherwise = Nothing


-- * String Parameters

glVendor, glRenderer, glVersion, glShadingLanguageVersion :: String
-- | The company responsible for this GL implementation. This name does not change from release to release.
glVendor = unsafePerformIO $ glGetString 0x1F00 >>= peekCString

-- | The renderer name typically specific to a particular configuration of a hardware platform. It does not change from release to release.
glRenderer = unsafePerformIO $ glGetString 0x1F01 >>= peekCString

-- | A version or release number of the form \"OpenGL ES (version number) (vendor-specific information)\"
glVersion = unsafePerformIO $ glGetString 0x1F02 >>= peekCString

-- | A version or release number for the shading language of the form \"OpenGL ES GLSL ES (version number) (vendor-specific information)\"
glShadingLanguageVersion = unsafePerformIO $ glGetString 0x8B8C >>= peekCString


-- * Two-valued Parameters

newtype GLParamP = GLParamP GLenum

glPair :: GLParamP -> (Float, Float)
glPair (GLParamP param) = unsafePerformIO $
	with (0 :: Double) $ \ptr -> do
		let p = castPtr ptr
		glGetFloatv param p
		r1 <- peek p
		r2 <- peekByteOff p 4
		return (r1, r2)

aliasedPointSizeRange, aliasedLineWidthRange, maxViewportDims :: GLParamP

-- | The (smallest, largest) supported sizes for points.
-- The smallest size must be \<= 1, and the largest size must be \>= 1.
aliasedPointSizeRange = GLParamP 0x846D

-- | The range of widths supported for aliased lines.
aliasedLineWidthRange = GLParamP 0x846E

-- | The maximum supported width and height of the viewport. These must be at least as large as the visible dimensions of the display being rendered to.
maxViewportDims = GLParamP 0x0D3A


-- * List Parameters

--GL_COMPRESSED_TEXTURE_FORMATS
--params returns a list of symbolic
--                        constants of length GL_NUM_COMPRESSED_TEXTURE_FORMATS 
--                        indicating which compressed texture formats are available.
--                        See glCompressedTexImage2D.
--compressedTextureFormats = GLParamL 0x86A3
--GL_SHADER_BINARY_FORMATS
--                        params returns a list of symbolic
--                        constants of length GL_NUM_SHADER_BINARY_FORMATS 
--                        indicating which shader binary formats are available.
--                        See glShaderBinary.
--shaderBinaryFormats = GLParamL 0x8DF8

-- ES3.0
--GL_PROGRAM_BINARY_FORMATS
--    params returns a list of symbolic constants of length GL_NUM_PROGRAM_BINARY_FORMATS indicating which program binary formats are available. See glProgramBinary.
--programBinaryFormats = GLParamL 0x87FF

-- * Integer Parameters

newtype GLParam = GLParam GLenum
-- glGetBooleanv glGetFloatv glGetIntegerv glGetInteger64v
-- glGetBooleani_v glGetIntegeri_v glGetInteger64i_v
-- As of 3.1, 188 + ext parameters are queriable.

--void glGetInteger64v( 	GLenum pname,
--  	GLint64 * data);
--void glGetBooleani_v( 	GLenum target,
--  	GLuint index,
--  	GLboolean * data);
-- GL_INVALID_ENUM is generated if pname is not an accepted value.

-- | Returns the implementation dependent value or 0 if unsupported.
glCap :: GLParam -> Int32
glCap (GLParam param) = unsafePerformIO $
	with 0 $ \ptr -> glGetIntegerv param ptr >> peek ptr

-- !!! add version for ES3+ or Ext, copy desc from man, least value for each version

{-- | a rough estimate of the largest 3D texture that the GL can handle. The value must be at least 256.
max3DTextureSize = GLParam 

GL_MAX_3D_TEXTURE_SIZE

	params returns one value,  See glTexImage3D.
GL_MAX_ARRAY_TEXTURE_LAYERS

	params returns one value. 
GL_MAX_AtomicCounter_BUFFER_BINDINGS

	params returns one value, the maximum number of atomic counter buffer binding points. The value must be at least 1. See glBindBuffer, glBindBufferBase, and glBindBufferRange.
GL_MAX_COLOR_ATTACHMENTS

	params returns one value, the maximum number of color attachment points in a framebuffer object. The value must be at least 4. See glFramebufferRenderbuffer and glFramebufferTexture2D.
GL_MAX_COMBINED_FRAGMENT_UniformCOMPONENTS

	params returns one value, the number of words for fragment shader uniform variables in all uniform blocks (including default). The value must be at least GL_MAX_FRAGMENT_UniformCOMPONENTS + GL_MAX_UNIFORM_BLOCK_SIZE * GL_MAX_FRAGMENT_UNIFORM_BLOCKS / 4. See glUniform.
GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS

	params returns one value, the maximum supported texture image units that can be used to access texture maps from the vertex shader and the fragment processor combined. If both the vertex shader and the fragment processing stage access the same texture image unit, then that counts as using two texture image units against this limit. The value must be at least 32. See glActiveTexture.
GL_MAX_COMBINED_UNIFORM_BLOCKS

	params returns one value, the maximum number of uniform blocks per program. The value must be at least 24. See glUniformBlockBinding.
GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS

	params returns one value, the number of words for vertex shader uniform variables in all uniform blocks (including default). The value must be at least . GL_MAX_VERTEX_UNIFORM_COMPONENTS + GL_MAX_UNIFORM_BLOCK_SIZE * GL_MAX_VERTEX_UNIFORM_BLOCKS / 4. See glUniform.
GL_MAX_CUBE_MAP_TEXTURE_SIZE

	params returns one value. The value gives a rough estimate of the largest cube-map texture that the GL can handle. The value must be at least 2048. See glTexImage2D.
GL_MAX_DRAW_BUFFERS

	params returns one value, the maximum number of simultaneous outputs that may be written in a fragment shader. The value must be at least 4. See glDrawBuffers.
GL_MAX_ELEMENT_INDEX

	params returns one value, the maximum index supported by the implementation. The value must be at least 2 24 - 1 .
GL_MAX_ELEMENTS_INDICES

	params returns one value, the recommended maximum number of vertex array indices. See glDrawRangeElements.
GL_MAX_ELEMENTS_VERTICES

	params returns one value, the recommended maximum number of vertex array vertices. See glDrawRangeElements.
GL_MAX_FRAGMENT_INPUT_COMPONENTS

	params returns one value, the maximum number of components of the inputs read by the fragment shader, which must be at least 60.
GL_MAX_FRAGMENT_UNIFORM_BLOCKS

	params returns one value, the maximum number of uniform blocks per fragment shader. The value must be at least 12. See glUniformBlockBinding.
GL_MAX_FRAGMENT_UNIFORM_COMPONENTS

	params returns one value, the maximum number of individual floating-point, integer, or boolean values that can be held in uniform variable storage for a fragment shader. The value must be at least 896. See glUniform.
GL_MAX_FRAGMENT_UNIFORM_VECTORS

	params returns one value, the maximum number of vector floating-point, integer, or boolean values that can be held in uniform variable storage for a fragment shader. The value must be at least 224. See glUniform.
GL_MAX_PROGRAM_TEXEL_OFFSET

	params returns one value, the maximum texel offset allowed in a texture lookup, which must be at least 7.
GL_MAX_RENDERBUFFER_SIZE

	params returns one value. The value indicates the maximum supported size for renderbuffers and must be at least 2048. See glFramebufferRenderbuffer.
GL_MAX_SAMPLE_MASK_WORDS

	params returns one value, the maximum number of sample mask words.
GL_MAX_SAMPLES

	params returns one value. The value indicates the maximum supported number of samples for multisampling. The value must be at least 4. See glGetInternalformativ.
GL_MAX_SERVER_WAIT_TIMEOUT

	params returns one value, the maximum glWaitSync timeout interval.
GL_MAX_SHADER_STORAGE_BLOCK_SIZE

	params returns one value, the maximum size in basic machine units of a shader storage block. The value must be at least 2 27 .
GL_MAX_TEXTURE_IMAGE_UNITS

	params returns one value, the maximum supported texture image units that can be used to access texture maps from the fragment shader. The value must be at least 16. See glActiveTexture.
GL_MAX_TEXTURE_LOD_BIAS

	params returns one value, the maximum, absolute value of the texture level-of-detail bias. The value must be at least 2.0.
GL_MAX_TEXTURE_SIZE

	params returns one value. The value gives a rough estimate of the largest texture that the GL can handle. The value must be at least 2048. See glTexImage2D.
GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS

	params returns one value, the maximum number of components which can be written to a single transform feedback buffer in interleaved mode. The value must be at least 64. See glTransformFeedbackVaryings.
GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS

	params returns one value, the maximum separate attributes or outputs which can be captured in separate transform feedback mode. The value must be at least 4. See glTransformFeedbackVaryings.
GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS

	params returns one value, the maximum number of components which can be written per attribute or output in separate transform feedback mode. The value must be at least 4. See glTransformFeedbackVaryings.
GL_MAX_UNIFORM_BLOCK_SIZE

	params returns one value, the maximum size in basic machine units of a uniform block. The value must be at least 16384. See glUniformBlockBinding.
GL_MAX_UNIFORM_BUFFER_BINDINGS

	params returns one value, the maximum number of uniform buffer binding points on the context, which must be at least 24.
GL_MAX_VARYING_COMPONENTS

	params returns one value, the number components for varying variables, which must be at least 60.
GL_MAX_VARYING_VECTORS

	params returns one value, the maximum number of interpolators available for processing varying variables used by vertex and fragment shaders. This value represents the number of vector values that can be interpolated; varying variables declared as matrices and arrays will consume multiple interpolators. The value must be at least 15.
GL_MAX_VERTEX_ATTRIBS

	params returns one value, the maximum number of 4-component generic vertex attributes accessible to a vertex shader. The value must be at least 16. See glVertexAttrib.
GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS

	params returns one value, the maximum supported texture image units that can be used to access texture maps from the vertex shader. The value may be at least 16. See glActiveTexture.
GL_MAX_VERTEX_OUTPUT_COMPONENTS

	params returns one value, the maximum number of components of output written by a vertex shader, which must be at least 64.
GL_MAX_VERTEX_UNIFORM_BLOCKS

	params returns one value, the maximum number of uniform blocks per vertex shader. The value must be at least 12. See glUniformBlockBinding.
GL_MAX_VERTEX_UNIFORM_COMPONENTS

	params returns one value, the maximum number of individual floating-point, integer, or boolean values that can be held in uniform variable storage for a vertex shader. The value must be at least 1024. See glUniform.
GL_MAX_VERTEX_UNIFORM_VECTORS

	params returns one value, the maximum number of vector floating-point, integer, or boolean values that can be held in uniform variable storage for a vertex shader. The value must be at least 256. See glUniform.
GL_MIN_PROGRAM_TEXEL_OFFSET

	params returns one value, the minimum texel offset allowed in a texture lookup, which must be at most -8.
-}


-- ** Since ES 2.0

-- | The format chosen by the implementation in which pixels may be read from the color buffer of the currently bound framebuffer in conjunction with GL_IMPLEMENTATION_COLOR_READ_TYPE. In addition to this implementation-dependent format/type pair, format GL_RGBA in conjunction with type GL_UNSIGNED_BYTE is always allowed by every                       implementation, regardless of the currently bound render surface. See glReadPixels.
implementationColorReadFormat = GLParam 0x8B9B

-- | The type chosen by the implementation with which pixels may be read from the color buffer of the currently bound framebuffer in conjunction with GL_IMPLEMENTATION_COLOR_READ_FORMAT. In addition to this implementation-dependent format/type pair, format  GL_RGBA in conjunction with type  GL_UNSIGNED_BYTE is always allowed by every implementation, regardless of the currently bound render surface. See glReadPixels.
implementationColorReadType = GLParam 0x8B9A

-- | The maximum supported texture image units that can be used to access texture maps from the vertex shader and the fragment processor combined. If both the vertex shader and the fragment processing stage access the same texture image unit, then that counts as using two texture image units against this limit. The minimum value is 8-32-32.
maxCombinedTextureImageUnits = GLParam 0x8B4D

-- | The value gives a rough estimate of the largest cube-map texture that the GL can handle. The minimum value is 16-2048-2048.
maxCubeMapTextureSize = GLParam 0x851C

-- | The maximum number of vector floating-point, integer, or boolean values that can be held in uniform variable storage for a fragment shader. The minimum value is 16-224-224.
maxFragmentUniformVectors = GLParam 0x8DFD

-- | The value indicates the maximum supported size for renderbuffers and must be at least 1-?-2048.
maxRenderbufferSize = GLParam 0x84E8

-- | The maximum supported texture image units that can be used to access texture maps from the fragment shader. The minimum value is 8-?-16.
maxTextureImageUnits = GLParam 0x8872

-- | The value gives a rough estimate of the largest texture that the GL can handle. The minimum value is 64-?-2048.
maxTextureSize = GLParam 0x0D33

-- | The maximum number of interpolators available for processing varying variables used by vertex and fragment shaders. This value represents the number of vector values that can be interpolated; varying variables declared as matrices and arrays will consume multiple interpolators. The minimum value is 8-?-15.
maxVaryingVectors = GLParam 0x8DFC

-- | The maximum number of 4-component generic vertex attributes accessible to a vertex shader. The minimum value is 8-?-16.
maxVertexAttribs = GLParam 0x8869

-- | The maximum supported texture image units that can be used to access texture maps from the vertex shader. The minimum value is 0-?-16.
maxVertexTextureImageUnits = GLParam 0x8B4C

-- | The maximum number of vector floating-point, integer, or boolean values that can be held in uniform variable storage for a vertex shader. The minimum value is 128-?-256.
maxVertexUniformVectors = GLParam 0x8DFB

-- | The number of available compressed texture formats.
-- The minimum value is 0-10-10.
numCompressedTextureFormats = GLParam 0x86A2

-- | The number of available shader binary formats.
-- The minimum value is 0-0-0.
-- Note: Not supported on most platforms.
numShaderBinaryFormats = GLParam 0x8DF9

-- | An estimate of the number of bits of subpixel resolution that are used to position rasterized geometry in window coordinates. The minimum value is 4-?-?.
subpixelBits = GLParam 0x0D50


-- ** Since ES 3.0

-- | A rough estimate of the largest 3D texture that the GL can handle. The minimum value is 0-256-256.
max3DTextureSize = GLParam 0x8073

-- | The value indicates the maximum number of layers allowed in an array texture. The minimum value is 0-256-256.
maxArrayTextureLayers = GLParam 0x88FF

-- | The maximum number of color attachment points in a framebuffer object. The minimum value is 0-4-4.
maxColorAttachments = GLParam 0x8CDF

-- | The number of words for fragment shader uniform variables in all uniform blocks (including default). The value must be at least GL_MAX_FRAGMENT_UNIFORM_COMPONENTS + GL_MAX_UNIFORM_BLOCK_SIZE * GL_MAX_FRAGMENT_UNIFORM_BLOCKS / 4.
maxCombinedFragmentUniformComponents = GLParam 0x8A33

-- | The maximum number of uniform blocks per program. The minimum value is 0-24-24.
maxCombinedUniformBlocks = GLParam 0x8A2E

-- | The number of words for vertex shader uniform variables in all uniform blocks (including default). The value must be at least GL_MAX_VERTEX_UNIFORM_COMPONENTS + GL_MAX_UNIFORM_BLOCK_SIZE * GL_MAX_VERTEX_UNIFORM_BLOCKS / 4.
maxCombinedVertexUniformComponents = GLParam 0x8A31

-- XXX 20 more




-- | The major version number of the OpenGL ES API supported by the
-- current context. Note: ES 2 does not support this param thus returns 0.
-- (ES2 -> 0, ES3 and ES3_1 -> 3)
majorVersion = GLParam 0x821B

-- | The minor version number of the OpenGL ES API supported by the
-- current context. (ES3 -> 0, ES3_1 -> 1)
minorVersion = GLParam 0x821C

-- | The number of extensions supported by the GL implementation for the current
-- context.
numExtensions = GLParam 0x821D

-- | __GL_OES_get_program_binary or ES3+__
-- The number of available program binary formats.
-- If the value is 0, program binary is not available.
-- The minimum value is 0-0-0.
numProgramBinaryFormats = GLParam 0x87FE


-- ** Since ES 3.1

-- | The maximum number of uniform blocks per compute shader. The value must be at least 14. See glUniformBlockBinding.
maxComputeUniformBlocks = GLParam 0x91BB

-- | The maximum supported texture image units that can be used to access texture maps from the compute shader. The value may be at least 16. See glActiveTexture.
maxComputeTextureImageUnits = GLParam 0x91BC

-- | The maximum number of image variables in compute shaders. The value must be at least 8.
maxComputeImageUniforms = GLParam 0x91BD

-- | 
maxComputeSharedMemorySize = GLParam 0x8262

-- | The maximum number of individual floating-point, integer, or boolean values that can be held in uniform variable storage for a compute shader. The value must be at least 1024. See glUniform.
maxComputeUniformComponents = GLParam 0x8263

-- | The maximum number of atomic counter buffers that may be accessed by a compute shader.
maxComputeAtomicCounterBuffers = GLParam 0x8264

-- | The maximum number of atomic counters available to compute shaders.
maxComputeAtomicCounters = GLParam 0x8265

-- | The number of words for compute shader uniform variables in all uniform blocks (including default). The value must be at least 1. See glUniform.
maxCombinedComputeUniformComponents = GLParam 0x8266

-- | The number of invocations in a single local work group (i.e., the product of the three dimensions) that may be dispatched to a compute shader.
maxComputeWorkGroupInvocations = GLParam 0x90EB

-- -- | The maximum number of work groups that may be dispatched to a compute shader. Indices 0, 1, and 2 correspond to the X, Y and Z dimensions, respectively.
-- maxComputeWorkGroupCount = GLParamIx? 0x91BE

-- -- | The maximum size of a work groups that may be used during compilation of a compute shader. Indices 0, 1, and 2 correspond to the X, Y and Z dimensions, respectively.
-- maxComputeWorkGroupSize = GLParamXYZ? 0x91BF

-- | The maximum number of explicitly assignable uniform locations, which must be at least 1024.
maxUniformLocations = GLParam 0x826E

-- | The maximum width for a framebuffer that has no attachments, which must be at least 16384. See glFramebufferParameteri.
maxFramebufferWidth = GLParam 0x9315

-- | The maximum height for a framebuffer that has no attachments, which must be at least 16384. See glFramebufferParameteri.
maxFramebufferHeight = GLParam 0x9316

-- | The maximum samples in a framebuffer that has no attachments, which must be at least 4. See glFramebufferParameteri.
maxFramebufferSamples = GLParam 0x9318

-- |
maxVertexAtomicCounterBuffers = GLParam 0x92CC

-- |
maxFragmentAtomicCountersBuffers = GLParam 0x92D0

-- |
maxCombinedAtomicCountersBuffers = GLParam 0x92D1

-- | The maximum number of atomic counters available to vertex shaders.
maxVertexAtomicCounters = GLParam 0x92D2

-- | The maximum number of atomic counters available to fragment shaders.
maxFragmentAtomicCounters = GLParam 0x92D6

-- | The maximum number of atomic counters available to all active shaders.
maxCombinedAtomicCounters = GLParam 0x92D7

-- |
maxAtomicCounterBufferSize = GLParam 0x92D8

-- |
maxAtomicCounterBufferBindings = GLParam 0x92DC

-- |
maxImageUnits = GLParam 0x8F38

-- |
maxVertexImageUniforms = GLParam 0x90CA

-- |
maxFragmentImageUniforms = GLParam 0x90CE

-- |
maxCombinedImageUniforms = GLParam 0x90CF

-- | The maximum number of active shader storage blocks that may be accessed by a vertex shader.
maxVertexShaderStorageBlocks = GLParam 0x90D6

-- | The maximum number of active shader storage blocks that may be accessed by a fragment shader.
maxFragmentShaderStorageBlocks = GLParam 0x90DA

-- | The maximum number of active shader storage blocks that may be accessed by a compute shader.
maxComputeShaderStorageBlocks = GLParam 0x90DB

-- | The maximum total number of active shader storage blocks that may be accessed by all active shaders.
maxCombinedShaderStorageBlocks = GLParam 0x90DC

-- | The maximum number of shader storage buffer binding points on the context, which must be at least 8.
maxShaderStorageBufferBindings = GLParam 0x90DD

-- |
maxShaderStorageBlockSize = GLParam 0x90DE

-- |
maxCombinedShaderOutputResources = GLParam 0x8F39

-- |
minProgramTextureGatherOffset = GLParam 0x8E5E

-- |
maxProgramTextureGatherOffset = GLParam 0x8E5F

-- | 
maxSampleMaskWords = GLParam 0x8E59

-- | The maximum number of samples in a color multisample texture.
maxColorTextureSamples = GLParam 0x910E

-- | 
maxDepthTextureSamples = GLParam 0x910F

-- | The maximum number of samples supported in integer format multisample buffers.
maxIntegerSamples = GLParam 0x9110

-- | The maximum offset that may be added to a vertex binding offset.
maxVertexAttribRelativeOffset = GLParam 0x82D9

-- | The maximum number of vertex buffers that may be bound.
maxVertexAttribBindings = GLParam 0x82DA

-- | /Undocumented constant/, perhaps /does not exist?/
maxVertexAttribStride = GLParam 0x82E5


-- ** Ext

--  | /(extname)/ (desc) Float?Int?
--maxTextureMaxAnisotropyExt = GLParam 0x84FF


-- * Shader Precision

-- | > shaderPrecision vertexShader highFp == (min, max, precision)
shaderPrecision :: (GLName -> a -> Shader) -> PrecisionType -> GL (Int32, Int32, Int32)
shaderPrecision shader (Prec precTyp) = 
	allocaArray 3 $ \range -> do
		let shaderType = case shader "" undefined of Shader x _ _ -> x
		let prec = advancePtr range 8
		glGetShaderPrecisionFormat shaderType precTyp range prec
		rmin <- peek range
		rmax <- peekByteOff range 4
		precision <- peek prec
		return (rmin, rmax, precision)

newtype PrecisionType = Prec GLenum
lowFp = Prec 0x8DF0
midiumFp = Prec 0x8DF1
highFp = Prec 0x8DF2
lowInt = Prec 0x8DF3
midiumInt = Prec 0x8DF4
highInt = Prec 0x8DF5

