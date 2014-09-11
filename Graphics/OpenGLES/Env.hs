-- | Retriving implementation depend constant values.
-- Do not call them before OpenGL initialization.
-- (glGetString could return null pointer!)
module Graphics.OpenGLES.Env where
import Foreign
import Foreign.C.String
import Graphics.OpenGLES.Base
import System.IO.Unsafe (unsafePerformIO)

-- * Extension detection

{-# NOINLINE glExtensions #-}
glExtensions :: [String]
glExtensions = unsafePerformIO $
	glGetString 0x1F03 >>= peekCString >>= return.words

{-# NOINLINE hasExt #-}
hasExt :: String -> Bool
hasExt ext = ext `elem` glExtensions
--hasExt :: CString -> Bool
--glGetString 0x1F03 >>= indexOf ext /= -1

hasES3 :: Bool
hasES3 = glEnv majorVersion > 2

extVAO :: Maybe (GLsizei -> Ptr GLuint -> GL (),
	GLuint -> GL (),
	GLsizei -> Ptr GLuint -> GL ())
extVAO | hasES3 =
		Just (glGenVertexArrays, glBindVertexArray, glDeleteVertexArrays)
        | hasExt "GL_OES_vertex_array_object" =
		Just (glGenVertexArraysOES, glBindVertexArrayOES, glDeleteVertexArraysOES)
        | otherwise = Nothing


-- * String Parameters

glVendor, glRenderer, glVersion, glShadingLanguageVersion :: String
glVendor = unsafePerformIO $ glGetString 0x1F00 >>= peekCString
glRenderer = unsafePerformIO $ glGetString 0x1F01 >>= peekCString
glVersion = unsafePerformIO $ glGetString 0x1F02 >>= peekCString
glShadingLanguageVersion = unsafePerformIO $ glGetString 0x8B8C >>= peekCString

isPVRTCsupported, isATITCsupported, isS3TCsupported :: Bool
isPVRTCsupported = hasExt "GL_IMG_texture_compression_pvrtc"
isATITCsupported = hasExt "GL_ATI_texture_compression_atitc"
isS3TCsupported = hasExt "GL_EXT_texture_compression_s3tc"


-- * Integer Parameters

newtype GLParam = GLParam GLenum deriving (Read, Show)
-- glGetBooleanv glGetFloatv glGetIntegerv glGetInteger64v
-- glGetBooleani_v glGetIntegeri_v glGetInteger64i_v
-- As of 3.1, 188 + ext parameters are querieable.

--void glGetInteger64v( 	GLenum pname,
--  	GLint64 * data);
--void glGetBooleani_v( 	GLenum target,
--  	GLuint index,
--  	GLboolean * data);
-- GL_INVALID_ENUM is generated if pname is not an accepted value.

-- ??? add failure value?
glEnv :: GLParam -> Int32
glEnv (GLParam param) = unsafePerformIO $
	with 0 $ \ptr -> glGetIntegerv param ptr >> peek ptr

-- !!! add version for ES3+ or Ext, copy desc from man, least value for each version

majorVersion, minorVersion, numExtensions,
	numProgramBinaryFormats :: GLParam

-- | /ES 3+/ The major version number of the OpenGL ES API supported by the
-- current context. Note: ES 2 does not support this param thus returns 0.
-- (ES2 -> 0, ES3 and ES3_1 -> 3)
majorVersion = GLParam 0x821B
-- | /ES 3+/ The minor version number of the OpenGL ES API supported by the
-- current context. (ES3 -> 0, ES3_1 -> 1)
minorVersion = GLParam 0x821C
-- | /ES 3+/ The number of extensions supported by the GL implementation for the current
-- context.
numExtensions = GLParam 0x821D
{-
GL_MAX_3D_TEXTURE_SIZE

    params returns one value, a rough estimate of the largest 3D texture that the GL can handle. The value must be at least 256. See glTexImage3D.
GL_MAX_ARRAY_TEXTURE_LAYERS

    params returns one value. The value indicates the maximum number of layers allowed in an array texture, and must be at least 256. See glTexImage2D.
GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS

    params returns one value, the maximum number of atomic counter buffer binding points. The value must be at least 1. See glBindBuffer, glBindBufferBase, and glBindBufferRange.
GL_MAX_COLOR_ATTACHMENTS

    params returns one value, the maximum number of color attachment points in a framebuffer object. The value must be at least 4. See glFramebufferRenderbuffer and glFramebufferTexture2D.
GL_MAX_COLOR_TEXTURE_SAMPLES

    params returns one value, the maximum number of samples in a color multisample texture.
GL_MAX_COMBINED_ATOMIC_COUNTERS

    params returns a single value, the maximum number of atomic counters available to all active shaders.
GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS

    params returns one value, the number of words for compute shader uniform variables in all uniform blocks (including default). The value must be at least 1. See glUniform.
GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS

    params returns one value, the number of words for fragment shader uniform variables in all uniform blocks (including default). The value must be at least GL_MAX_FRAGMENT_UNIFORM_COMPONENTS + GL_MAX_UNIFORM_BLOCK_SIZE * GL_MAX_FRAGMENT_UNIFORM_BLOCKS / 4. See glUniform.
GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS

    params returns one value, the maximum total number of active shader storage blocks that may be accessed by all active shaders.
GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS

    params returns one value, the maximum supported texture image units that can be used to access texture maps from the vertex shader and the fragment processor combined. If both the vertex shader and the fragment processing stage access the same texture image unit, then that counts as using two texture image units against this limit. The value must be at least 32. See glActiveTexture.
GL_MAX_COMBINED_UNIFORM_BLOCKS

    params returns one value, the maximum number of uniform blocks per program. The value must be at least 24. See glUniformBlockBinding.
GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS

    params returns one value, the number of words for vertex shader uniform variables in all uniform blocks (including default). The value must be at least . GL_MAX_VERTEX_UNIFORM_COMPONENTS + GL_MAX_UNIFORM_BLOCK_SIZE * GL_MAX_VERTEX_UNIFORM_BLOCKS / 4. See glUniform.
GL_MAX_COMPUTE_ATOMIC_COUNTERS

    params returns a single value, the maximum number of atomic counters available to compute shaders.
GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS

    params returns a single value, the maximum number of atomic counter buffers that may be accessed by a compute shader.
GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS

    params returns one value, the maximum number of active shader storage blocks that may be accessed by a compute shader.
GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS

    params returns one value, the maximum supported texture image units that can be used to access texture maps from the compute shader. The value may be at least 16. See glActiveTexture.
GL_MAX_COMPUTE_UNIFORM_BLOCKS

    params returns one value, the maximum number of uniform blocks per compute shader. The value must be at least 14. See glUniformBlockBinding.
GL_MAX_COMPUTE_UNIFORM_COMPONENTS

    params returns one value, the maximum number of individual floating-point, integer, or boolean values that can be held in uniform variable storage for a compute shader. The value must be at least 1024. See glUniform.
GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS

    params returns one value, the number of invocations in a single local work group (i.e., the product of the three dimensions) that may be dispatched to a compute shader.
GL_MAX_COMPUTE_WORK_GROUP_COUNT

    Accepted by the indexed versions of glGet. params the maximum number of work groups that may be dispatched to a compute shader. Indices 0, 1, and 2 correspond to the X, Y and Z dimensions, respectively.
GL_MAX_COMPUTE_WORK_GROUP_SIZE

    Accepted by the indexed versions of glGet. params the maximum size of a work groups that may be used during compilation of a compute shader. Indices 0, 1, and 2 correspond to the X, Y and Z dimensions, respectively.
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
GL_MAX_FRAGMENT_ATOMIC_COUNTERS

    params returns a single value, the maximum number of atomic counters available to fragment shaders.
GL_MAX_FRAGMENT_INPUT_COMPONENTS

    params returns one value, the maximum number of components of the inputs read by the fragment shader, which must be at least 60.
GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS

    params returns one value, the maximum number of active shader storage blocks that may be accessed by a fragment shader.
GL_MAX_FRAGMENT_UNIFORM_BLOCKS

    params returns one value, the maximum number of uniform blocks per fragment shader. The value must be at least 12. See glUniformBlockBinding.
GL_MAX_FRAGMENT_UNIFORM_COMPONENTS

    params returns one value, the maximum number of individual floating-point, integer, or boolean values that can be held in uniform variable storage for a fragment shader. The value must be at least 896. See glUniform.
GL_MAX_FRAGMENT_UNIFORM_VECTORS

    params returns one value, the maximum number of vector floating-point, integer, or boolean values that can be held in uniform variable storage for a fragment shader. The value must be at least 224. See glUniform.
GL_MAX_FRAMEBUFFER_HEIGHT

    params returns one value, the maximum height for a framebuffer that has no attachments, which must be at least 16384. See glFramebufferParameteri.
GL_MAX_FRAMEBUFFER_SAMPLES

    params returns one value, the maximum samples in a framebuffer that has no attachments, which must be at least 4. See glFramebufferParameteri.
GL_MAX_FRAMEBUFFER_WIDTH

    params returns one value, the maximum width for a framebuffer that has no attachments, which must be at least 16384. See glFramebufferParameteri.
GL_MAX_INTEGER_SAMPLES

    params returns one value, the maximum number of samples supported in integer format multisample buffers.
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
GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS

    params returns one value, the maximum number of shader storage buffer binding points on the context, which must be at least 8.
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
GL_MAX_UNIFORM_LOCATIONS

    params returns one value, the maximum number of explicitly assignable uniform locations, which must be at least 1024.
GL_MAX_VARYING_COMPONENTS

    params returns one value, the number components for varying variables, which must be at least 60.
GL_MAX_VARYING_VECTORS

    params returns one value, the maximum number of interpolators available for processing varying variables used by vertex and fragment shaders. This value represents the number of vector values that can be interpolated; varying variables declared as matrices and arrays will consume multiple interpolators. The value must be at least 15.
GL_MAX_VERTEX_ATOMIC_COUNTERS

    params returns a single value, the maximum number of atomic counters available to vertex shaders.
GL_MAX_VERTEX_ATTRIB_BINDINGS

    params returns a single integer value containing the maximum number of vertex buffers that may be bound.
GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET

    params returns a single integer value containing the maximum offset that may be added to a vertex binding offset.
GL_MAX_VERTEX_ATTRIBS

    params returns one value, the maximum number of 4-component generic vertex attributes accessible to a vertex shader. The value must be at least 16. See glVertexAttrib.
GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS

    params returns one value, the maximum number of active shader storage blocks that may be accessed by a vertex shader.
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
GL_NUM_COMPRESSED_TEXTURE_FORMATS

    params returns a single integer value indicating the number of available compressed texture formats. The minimum value is 10. See glCompressedTexImage2D.
-}
-- | /GL_OES_get_program_binary or ES3+/
-- The number of available program binary formats.
-- If the value is 0, program binary is not available.
numProgramBinaryFormats = GLParam 0x87FE
-- | The number of available shader binary formats. The minimum value is 0.
-- Note: Not supported on most platforms.
--numShaderBinaryFormats = GLParam 0x8DF9

-- * Ranged Parameters

newtype GLParamP = GLParamP GLenum deriving (Read, Show)

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
-- | The maximum supported width and height of the viewport.
-- These must be at least as large as the visible dimensions of the display
-- being rendered to.
maxViewportDims = GLParamP 0x0D3A

