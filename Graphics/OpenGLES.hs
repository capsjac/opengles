{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- | OpenGL ES (ES for Embed Systems) 2.0
-- <http://www.khronos.org/opengles/sdk/docs/reference_cards/OpenGL-ES-2_0-Reference-card.pdf>
-- <http://www.khronos.org/files/opengles3-quick-reference-card.pdf>
-- 
-- ANGLE: OpenGL ES 2.0 backend for Windows.
-- http://code.google.com/p/angleproject/

module Graphics.OpenGLES where
import Control.Applicative
import Data.Bits ((.|.))
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.EGL (eglGetProcAddress)
import Graphics.OpenGLES.Types
import Unsafe.Coerce

isGLProcAvailable :: String -> Bool
isGLProcAvailable name = eglGetProcAddress name /= nullFunPtr

--getopenglversion, dummy call or throwifnull

#define GL_PROC(_procname, _typ) \
foreign import ccall unsafe "dynamic" unwrap_/**/_procname :: FunPtr (_typ) -> _typ; \
_procname :: _typ; \
_procname = unwrap_/**/_procname (eglGetProcAddress "_procname"); 

-- foreign import ccall unsafe "dynamic"
--   unwrap_glActiveTexture :: FunPtr (GLenum -> IO ()) -> GLenum -> IO ();
-- glActiveTexture :: GLenum -> IO ();
-- glActiveTexture = unwrap_glActiveTexture (eglGetProcAddress "glActiveTexture");

-- * OpenGL ES 2.0

GL_PROC(glActiveTexture, GLenum -> IO ())
GL_PROC(glAttachShader, GLuint -> GLuint -> IO ())
GL_PROC(glBindAttribLocation, GLuint -> GLuint -> CString -> IO ())
GL_PROC(glBindBuffer, GLenum -> GLuint -> IO ())
GL_PROC(glBindFramebuffer, GLenum -> GLuint -> IO ())
GL_PROC(glBindRenderbuffer, GLenum -> GLuint -> IO ())
GL_PROC(glBindTexture, GLenum -> GLuint -> IO ())
GL_PROC(glBlendColor, GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ())
GL_PROC(glBlendEquation, GLenum -> IO ())
GL_PROC(glBlendEquationSeparate, GLenum -> GLenum -> IO ())
GL_PROC(glBlendFunc, GLenum -> GLenum -> IO ())
GL_PROC(glBlendFuncSeparate, GLenum -> GLenum -> GLenum -> GLenum -> IO ())
GL_PROC(glBufferData, GLenum -> GLsizeiptr -> Ptr () -> GLenum -> IO ())
GL_PROC(glBufferSubData, GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> IO ())
GL_PROC(glCheckFramebufferStatus, GLenum -> IO GLenum)
GL_PROC(glClear, GLbitfield -> IO ())
GL_PROC(glClearColor, GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ())
GL_PROC(glClearDepthf, GLclampf -> IO ())
GL_PROC(glClearStencil, GLint -> IO ())
GL_PROC(glColorMask, GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
GL_PROC(glCompileShader, GLuint -> IO ())
GL_PROC(glCompressedTexImage2D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
GL_PROC(glCompressedTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
GL_PROC(glCopyTexImage2D, GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
GL_PROC(glCopyTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GL_PROC(glCreateProgram, IO GLuint)
GL_PROC(glCreateShader, GLenum -> IO GLuint)
GL_PROC(glCullFace, GLenum -> IO ())
GL_PROC(glDeleteBuffers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glDeleteFramebuffers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glDeleteProgram, GLuint -> IO ())
GL_PROC(glDeleteRenderbuffers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glDeleteShader, GLuint -> IO ())
GL_PROC(glDeleteTextures, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glDepthFunc, GLenum -> IO ())
GL_PROC(glDepthMask, GLboolean -> IO ())
GL_PROC(glDepthRangef, GLclampf -> GLclampf -> IO ())
GL_PROC(glDetachShader, GLuint -> GLuint -> IO ())
GL_PROC(glDisable, GLenum -> IO ())
GL_PROC(glDisableVertexAttribArray, GLuint -> IO ())
GL_PROC(glDrawArrays, GLenum -> GLint -> GLsizei -> IO ())
GL_PROC(glDrawElements, GLenum -> GLsizei -> GLenum -> Ptr () -> IO ())
GL_PROC(glEnable, GLenum -> IO ())
GL_PROC(glEnableVertexAttribArray, GLuint -> IO ())
GL_PROC(glFinish, IO ())
GL_PROC(glFlush, IO ())
GL_PROC(glFramebufferRenderbuffer, GLenum -> GLenum -> GLenum -> GLuint -> IO ())
GL_PROC(glFramebufferTexture2D, GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
GL_PROC(glFrontFace, GLenum -> IO ())
GL_PROC(glGenBuffers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glGenerateMipmap, GLenum -> IO ())
GL_PROC(glGenFramebuffers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glGenRenderbuffers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glGenTextures, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glGetActiveAttrib, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
GL_PROC(glGetActiveUniform, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
GL_PROC(glGetAttachedShaders, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glGetAttribLocation, GLuint -> CString -> IO GLint)
GL_PROC(glGetBooleanv, GLenum -> Ptr GLboolean -> IO ())
GL_PROC(glGetBufferParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetError, IO GLenum)
GL_PROC(glGetFloatv, GLenum -> Ptr GLfloat -> IO ())
GL_PROC(glGetFramebufferAttachmentParameteriv, GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetIntegerv, GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetProgramiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetProgramInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
GL_PROC(glGetRenderbufferParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetShaderiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetShaderInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
GL_PROC(glGetShaderPrecisionFormat, GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ())
GL_PROC(glGetShaderSource, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
GL_PROC(glGetString, GLenum -> IO CString)
GL_PROC(glGetTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> IO ())
GL_PROC(glGetTexParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetUniformfv, GLuint -> GLint -> Ptr GLfloat -> IO ())
GL_PROC(glGetUniformiv, GLuint -> GLint -> Ptr GLint -> IO ())
GL_PROC(glGetUniformLocation, GLuint -> CString -> IO GLint)
GL_PROC(glGetVertexAttribfv, GLuint -> GLenum -> Ptr GLfloat -> IO ())
GL_PROC(glGetVertexAttribiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetVertexAttribPointerv, GLuint -> GLenum -> Ptr (Ptr ()) -> IO ())
GL_PROC(glHint, GLenum -> GLenum -> IO ())
GL_PROC(glIsBuffer, GLuint -> IO GLboolean)
GL_PROC(glIsEnabled, GLenum -> IO GLboolean)
GL_PROC(glIsFramebuffer, GLuint -> IO GLboolean)
GL_PROC(glIsProgram, GLuint -> IO GLboolean)
GL_PROC(glIsRenderbuffer, GLuint -> IO GLboolean)
GL_PROC(glIsShader, GLuint -> IO GLboolean)
GL_PROC(glIsTexture, GLuint -> IO GLboolean)
GL_PROC(glLineWidth, GLfloat -> IO ())
GL_PROC(glLinkProgram, GLuint -> IO ())
GL_PROC(glPixelStorei, GLenum -> GLint -> IO ())
GL_PROC(glPolygonOffset, GLfloat -> GLfloat -> IO ())
GL_PROC(glReadPixels, GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
GL_PROC(glReleaseShaderCompiler, IO ())
GL_PROC(glRenderbufferStorage, GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
GL_PROC(glSampleCoverage, GLclampf -> GLboolean -> IO ())
GL_PROC(glScissor, GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GL_PROC(glShaderBinary, GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> IO ())
GL_PROC(glShaderSource, GLuint -> GLsizei -> Ptr CString -> Ptr GLint -> IO ())
GL_PROC(glStencilFunc, GLenum -> GLint -> GLuint -> IO ())
GL_PROC(glStencilFuncSeparate, GLenum -> GLenum -> GLint -> GLuint -> IO ())
GL_PROC(glStencilMask, GLuint -> IO ())
GL_PROC(glStencilMaskSeparate, GLenum -> GLuint -> IO ())
GL_PROC(glStencilOp, GLenum -> GLenum -> GLenum -> IO ())
GL_PROC(glStencilOpSeparate, GLenum -> GLenum -> GLenum -> GLenum -> IO ())
GL_PROC(glTexImage2D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
GL_PROC(glTexParameterf, GLenum -> GLenum -> GLfloat -> IO ())
GL_PROC(glTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> IO ())
GL_PROC(glTexParameteri, GLenum -> GLenum -> GLint -> IO ())
GL_PROC(glTexParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
GL_PROC(glUniform1f, GLint -> GLfloat -> IO ())
GL_PROC(glUniform1fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_PROC(glUniform1i, GLint -> GLint -> IO ())
GL_PROC(glUniform1iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GL_PROC(glUniform2f, GLint -> GLfloat -> GLfloat -> IO ())
GL_PROC(glUniform2fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_PROC(glUniform2i, GLint -> GLint -> GLint -> IO ())
GL_PROC(glUniform2iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GL_PROC(glUniform3f, GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
GL_PROC(glUniform3fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_PROC(glUniform3i, GLint -> GLint -> GLint -> GLint -> IO ())
GL_PROC(glUniform3iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GL_PROC(glUniform4f, GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
GL_PROC(glUniform4fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_PROC(glUniform4i, GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
GL_PROC(glUniform4iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GL_PROC(glUniformMatrix2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUniformMatrix3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUniformMatrix4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUseProgram, GLuint -> IO ())
GL_PROC(glValidateProgram, GLuint -> IO ())
GL_PROC(glVertexAttrib1f, GLuint -> GLfloat -> IO ())
GL_PROC(glVertexAttrib1fv, GLuint -> Ptr GLfloat -> IO ())
GL_PROC(glVertexAttrib2f, GLuint -> GLfloat -> GLfloat -> IO ())
GL_PROC(glVertexAttrib2fv, GLuint -> Ptr GLfloat -> IO ())
GL_PROC(glVertexAttrib3f, GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
GL_PROC(glVertexAttrib3fv, GLuint -> Ptr GLfloat -> IO ())
GL_PROC(glVertexAttrib4f, GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
GL_PROC(glVertexAttrib4fv, GLuint -> Ptr GLfloat -> IO ())
GL_PROC(glVertexAttribPointer, GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> IO ())
GL_PROC(glViewport, GLint -> GLint -> GLsizei -> GLsizei -> IO ())

-- * OpenGL ES 3.0

GL_PROC(glReadBuffer, GLenum -> IO ())
GL_PROC(glDrawRangeElements, GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr (()) -> IO ())
GL_PROC(glTexImage3D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr (()) -> IO ())
GL_PROC(glTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
GL_PROC(glCopyTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GL_PROC(glCompressedTexImage3D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
GL_PROC(glCompressedTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
GL_PROC(glGenQueries, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glDeleteQueries, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glIsQuery, GLuint -> IO GLboolean)
GL_PROC(glBeginQuery, GLenum -> GLuint -> IO ())
GL_PROC(glEndQuery, GLenum -> IO ())
GL_PROC(glGetQueryiv, GLenum -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetQueryObjectuiv, GLuint -> GLenum -> Ptr GLuint -> IO ())
GL_PROC(glUnmapBuffer, GLenum -> IO GLboolean)
GL_PROC(glGetBufferPointerv, GLenum -> GLenum -> Ptr (Ptr ()) -> IO ())
GL_PROC(glDrawBuffers, GLsizei -> Ptr GLenum -> IO ())
GL_PROC(glUniformMatrix2x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUniformMatrix3x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUniformMatrix2x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUniformMatrix4x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUniformMatrix3x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glUniformMatrix4x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_PROC(glBlitFramebuffer, GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
GL_PROC(glRenderbufferStorageMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
GL_PROC(glFramebufferTextureLayer, GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
GL_PROC(glMapBufferRange, GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ()))
GL_PROC(glFlushMappedBufferRange, GLenum -> GLintptr -> GLsizeiptr -> IO ())
GL_PROC(glBindVertexArray, GLuint -> IO ())
GL_PROC(glDeleteVertexArrays, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glGenVertexArrays, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glIsVertexArray, GLuint -> IO GLboolean)
GL_PROC(glGetIntegeri_v, GLenum -> GLuint -> Ptr GLint -> IO ())
GL_PROC(glBeginTransformFeedback, GLenum -> IO ())
GL_PROC(glEndTransformFeedback, IO ())
GL_PROC(glBindBufferRange, GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
GL_PROC(glBindBufferBase, GLenum -> GLuint -> GLuint -> IO ())
GL_PROC(glTransformFeedbackVaryings, GLuint -> GLsizei -> Ptr CString -> GLenum -> IO ())
GL_PROC(glGetTransformFeedbackVarying, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
GL_PROC(glVertexAttribIPointer, GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
GL_PROC(glGetVertexAttribIiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetVertexAttribIuiv, GLuint -> GLenum -> Ptr GLuint -> IO ())
GL_PROC(glVertexAttribI4i, GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
GL_PROC(glVertexAttribI4ui, GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
GL_PROC(glVertexAttribI4iv, GLuint -> Ptr GLint -> IO ())
GL_PROC(glVertexAttribI4uiv, GLuint -> Ptr GLuint -> IO ())
GL_PROC(glGetUniformuiv, GLuint -> GLint -> Ptr GLuint -> IO ())
GL_PROC(glGetFragDataLocation, GLuint -> CString-> IO GLint)
GL_PROC(glUniform1ui, GLint -> GLuint -> IO ())
GL_PROC(glUniform2ui, GLint -> GLuint -> GLuint -> IO ())
GL_PROC(glUniform3ui, GLint -> GLuint -> GLuint -> GLuint -> IO ())
GL_PROC(glUniform4ui, GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
GL_PROC(glUniform1uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glUniform2uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glUniform3uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glUniform4uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glClearBufferiv, GLenum -> GLint -> Ptr GLint -> IO ())
GL_PROC(glClearBufferuiv, GLenum -> GLint -> Ptr GLuint -> IO ())
GL_PROC(glClearBufferfv, GLenum -> GLint -> Ptr GLfloat -> IO ())
GL_PROC(glClearBufferfi, GLenum -> GLint -> GLfloat -> GLint -> IO ())
GL_PROC(glGetStringi, GLenum -> GLuint -> IO CString)
GL_PROC(glCopyBufferSubData, GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
GL_PROC(glGetUniformIndices, GLuint -> GLsizei -> Ptr CString -> Ptr GLuint -> IO ())
GL_PROC(glGetActiveUniformsiv, GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetUniformBlockIndex, GLuint -> Ptr GLchar -> IO GLuint)
GL_PROC(glGetActiveUniformBlockiv, GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetActiveUniformBlockName, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
GL_PROC(glUniformBlockBinding, GLuint -> GLuint -> GLuint -> IO ())
GL_PROC(glDrawArraysInstanced, GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
GL_PROC(glDrawElementsInstanced, GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> IO ())
GL_PROC(glFenceSync, GLenum -> GLbitfield -> IO ())
GL_PROC(glIsSync, GLsync -> IO GLsync)
GL_PROC(glDeleteSync, GLsync -> IO GLboolean)
GL_PROC(glClientWaitSync, GLsync -> GLbitfield -> GLuint64 -> IO GLenum)
GL_PROC(glWaitSync, GLsync -> GLbitfield -> GLuint64 -> IO ())
GL_PROC(glGetInteger64v, GLenum -> Ptr GLint64 -> IO ())
GL_PROC(glGetSynciv, GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
GL_PROC(glGetInteger64i_v, GLenum -> GLuint -> Ptr GLint64 -> IO ())
GL_PROC(glGetBufferParameteri64v, GLenum -> GLenum -> Ptr GLint64 -> IO ())
GL_PROC(glGenSamplers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glDeleteSamplers, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glIsSampler, GLuint -> IO GLboolean)
GL_PROC(glBindSampler, GLuint -> GLuint -> IO ())
GL_PROC(glSamplerParameteri, GLuint -> GLenum -> GLint -> IO ())
GL_PROC(glSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glSamplerParameterf, GLuint -> GLenum -> GLfloat -> IO ())
GL_PROC(glSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> IO ())
GL_PROC(glGetSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_PROC(glGetSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> IO ())
GL_PROC(glVertexAttribDivisor, GLuint -> GLuint -> IO ())
GL_PROC(glBindTransformFeedback, GLenum -> GLuint -> IO ())
GL_PROC(glDeleteTransformFeedbacks, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glGenTransformFeedbacks, GLsizei -> Ptr GLuint -> IO ())
GL_PROC(glIsTransformFeedback, GLuint -> IO GLboolean)
GL_PROC(glPauseTransformFeedback, IO ())
GL_PROC(glResumeTransformFeedback, IO ())
GL_PROC(glGetProgramBinary, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> IO ())
GL_PROC(glProgramBinary, GLuint -> GLenum -> Ptr () -> GLsizei -> IO ())
GL_PROC(glProgramParameteri, GLuint -> GLenum -> GLint -> IO ())
GL_PROC(glInvalidateFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> IO ())
GL_PROC(glInvalidateSubFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GL_PROC(glTexStorage2D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
GL_PROC(glTexStorage3D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
GL_PROC(glGetInternalformativ, GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ())

-- ** Errors
data GLError = NoError | InvalidEnum | InvalidValue | InvalidOperation
             | OutOfMemory | InvalidFrameBufferOperation deriving Show

getError :: IO GLError
getError = unMarshal <$> glGetError
	where unMarshal x = case x of
		0x0000 -> NoError
		0x0500 -> InvalidEnum
		0x0501 -> InvalidValue
		0x0502 -> InvalidOperation
		0x0505 -> OutOfMemory
		0x0506 -> InvalidFrameBufferOperation

-- ** Buffer Objects
-- | EnableCap
data OpenGLCapability =
	  Texture2D
	| CullFace
	| Blend
	| Dither
	| StencilTest
	| DepthTest
	| ScissorTest
	| PolygonOffsetFill
	| SampleAlphaToCoverage
	| SampleCoverage
	| PrimitiveRestartFixedIndex -- ^ ES 3.0

instance Marshal OpenGLCapability where
	marshal x = case x of
		Texture2D             -> 0x0DE1
		CullFace              -> 0x0B44
		Blend                 -> 0x0BE2
		Dither                -> 0x0BD0
		StencilTest           -> 0x0B90
		DepthTest             -> 0x0B71
		ScissorTest           -> 0x0C11
		PolygonOffsetFill     -> 0x8037
		SampleAlphaToCoverage -> 0x809E
		SampleCoverage        -> 0x80A0
		PrimitiveRestartFixedIndex -> 0x8D69

enable :: OpenGLCapability -> IO ()
enable = glEnable . marshal

disable :: OpenGLCapability -> IO ()
disable = glDisable . marshal

isEnabled :: OpenGLCapability -> IO Bool
isEnabled = liftA (/= 0) . glIsEnabled . marshal

-- | glClear
clearBuffer :: Bool -- ^ Clear color buffer
            -> Bool -- ^ Clear depth buffer
            -> Bool -- ^ Clear stencil buffer
            -> IO ()
clearBuffer c d s = glClear $ (if d then 0x100 else 0)
  .|. (if s then 0x400 else 0) .|. (if c then 0x4000 else 0)

getGLVendor = glGetString 0x1F00 >>= peekCString
getGLRenderer = glGetString 0x1F01 >>= peekCString
getGLVersion = glGetString 0x1F02 >>= peekCString
getGLExtensions = words <$> (glGetString 0x1F03 >>= peekCString)
getGLShadingLanguageVersion = glGetString 0x8B8C >>= peekCString

data CullFaceMode = Front | Back | FrontAndBack

instance Marshal CullFaceMode where
	marshal Front        = 0x0404
	marshal Back         = 0x0405
	marshal FrontAndBack = 0x0408

stencilMaskSeparate :: CullFaceMode -> GLuint -> IO ()
stencilMaskSeparate face mask = glStencilMaskSeparate (marshal face) mask

data HintTarget = GenerateMipmapHint
                | FragmentShaderDerivativeHint -- ^ ES 3.0

instance Marshal HintTarget where
	marshal GenerateMipmapHint = 0x8192
	marshal FragmentShaderDerivativeHint = 0x8B8B

data HintMode = DontCare | Fastest | Nicest

instance Marshal HintMode where
	marshal DontCare = 0x1100
	marshal Fastest  = 0x1101
	marshal Nicest   = 0x1102

hint :: HintTarget -> HintMode -> IO ()
hint target hintmode = glHint (marshal target) (marshal hintmode) 

data StencilFunction =
	  SFNever | SFLess | SFEqual | SFLEqual | SFGreater
	| SFNotEqual | SFGEqual | SFAlways

instance Marshal StencilFunction where
	marshal x = case x of
		SFNever    -> 0x0200
		SFLess     -> 0x0201
		SFEqual    -> 0x0202
		SFLEqual   -> 0x0203
		SFGreater  -> 0x0204
		SFNotEqual -> 0x0205
		SFGEqual   -> 0x0206
		SFAlways   -> 0x0207

depthFunc :: StencilFunction -> IO ()
depthFunc = glDepthFunc . marshal

data StencilOp =
	  OpZero | OpKeep | OpReplace | OpIncr
	| OpDecr | OpInvert | OpIncrWrap | OpDecrWrap

instance Marshal StencilOp where
	marshal x = case x of
		OpZero     -> 0x0000
		OpKeep     -> 0x1E00
		OpReplace  -> 0x1E01
		OpIncr     -> 0x1E02
		OpDecr     -> 0x1E03
		OpInvert   -> 0x150A
		OpIncrWrap -> 0x8507
		OpDecrWrap -> 0x8508

stencilOp :: StencilOp -> StencilOp -> StencilOp -> IO ()
stencilOp sfail dpfail dppass =
	glStencilOp (marshal sfail) (marshal dpfail) (marshal dppass)

stencilOpSeparate :: CullFaceMode -> StencilOp -> StencilOp -> StencilOp -> IO ()
stencilOpSeparate face sfail dpfail dppass =
	glStencilOpSeparate (marshal face) (marshal sfail)
	                    (marshal dpfail) (marshal dppass)

data FrontFaceDirection = CW | CCW
frontFace CW = glFrontFace 0x0900
frontFace CCW = glFrontFace 0x0901

cullFace :: CullFaceMode -> IO ()
cullFace = glCullFace . marshal

-- | An abstraction layer for glGen*, glBind*, glIs* and glDelete*
class ServerObject a where
	genObjects :: Int -> IO [a]
	bindObject :: (BindTarget a b) => b -> a -> IO ()
	isObject :: a -> IO Bool
	deleteObjects :: [a] -> IO ()

class (Marshal b) => BindTarget a b

data Buffer = Buffer { unBuffer :: GLuint }

instance ServerObject Buffer where
	genObjects n = allocaArray n $ \arr -> do
		glGenBuffers (fromIntegral n) arr
		map Buffer <$> peekArray n arr
	bindObject target (Buffer x) = glBindBuffer (marshal target) x
	isObject (Buffer x) = return . (== 1) =<< glIsBuffer x
	deleteObjects xs = withArray (map unBuffer xs) $ \arr ->
		glDeleteBuffers (fromIntegral $ length xs) arr

data BufferTarget = ArrayBuffer
                  | ElementArrayBuffer
                  | PixelPackBuffer -- ^ ES 3.0
                  | PixelUnpackBuffer -- ^ ES 3.0
                  | CopyReadBuffer -- ^ ES 3.0
                  | CopyWriteBuffer -- ^ ES 3.0
-- | ES 3.0
data BufferTarget' = TransformFeedbackBuffer | UniformBuffer

instance BindTarget	Buffer BufferTarget
instance BindTarget	Buffer BufferTarget'
instance Marshal BufferTarget where
	marshal x = case x of
		ArrayBuffer -> 0x8892
		ElementArrayBuffer -> 0x8893
		PixelPackBuffer -> 0x88EB
		PixelUnpackBuffer -> 0x88EC
		CopyReadBuffer -> 0x8F36
		CopyWriteBuffer -> 0x8F37
instance Marshal BufferTarget' where
	marshal TransformFeedbackBuffer = 0x8C8E
	marshal UniformBuffer = 0x8A11


data Framebuffer = FramebufferObj { unFramebuffer :: GLuint }

instance ServerObject Framebuffer where
	genObjects n = allocaArray n $ \arr -> do
		glGenFramebuffers (fromIntegral n) arr
		map FramebufferObj <$> peekArray n arr
	bindObject target (FramebufferObj x) =
		glBindFramebuffer (marshal target) x
	isObject (FramebufferObj x) =
		return . (== 1) =<< glIsFramebuffer x
	deleteObjects xs = withArray (map unFramebuffer xs) $ \arr ->
		glDeleteFramebuffers (fromIntegral $ length xs) arr

data FramebufferTarget = Framebuffer
data FramebufferTarget' = Framebuffer'
                        | DrawFramebuffer -- ^ ES 3.0
                        | ReadFramebuffer -- ^ ES 3.0
instance BindTarget	Framebuffer FramebufferTarget
instance Marshal FramebufferTarget where
	marshal Framebuffer = 0x8D40


data Renderbuffer = RenderbufferObj { unRenderbuffer :: GLuint }

instance ServerObject Renderbuffer where
	genObjects n = allocaArray n $ \arr -> do
		glGenRenderbuffers (fromIntegral n) arr
		map RenderbufferObj <$> peekArray n arr
	bindObject target (RenderbufferObj x) =
		glBindRenderbuffer (marshal target) x
	isObject (RenderbufferObj x) =
		return . (== 1) =<< glIsRenderbuffer x
	deleteObjects xs = withArray (map unRenderbuffer xs) $ \arr ->
		glDeleteRenderbuffers (fromIntegral $ length xs) arr

data RenderbufferTarget = Renderbuffer
instance BindTarget	Renderbuffer RenderbufferTarget
instance Marshal RenderbufferTarget where
	marshal Renderbuffer = 0x8D40


data Texture = TextureObj { unTexture :: GLuint }

instance ServerObject Texture where
	genObjects n = allocaArray n $ \arr -> do
		glGenTextures (fromIntegral n) arr
		map TextureObj <$> peekArray n arr
	bindObject target (TextureObj x) =
		glBindTexture (marshal target) x
	isObject (TextureObj x) =
		return . (== 1) =<< glIsTexture x
	deleteObjects xs = withArray (map unTexture xs) $ \arr ->
		glDeleteTextures (fromIntegral $ length xs) arr

data TextureTarget =
	  Tex2D
	| TexCubeMap
	| TexCubeMapPosX | TexCubeMapPosY | TexCubeMapPosZ
	| TexCubeMapNegX | TexCubeMapNegY | TexCubeMapNegZ
	| Tex3D -- ^ ES 3.0
	| Tex2DArray -- ^ ES 3.0

instance BindTarget	Texture TextureTarget
instance Marshal TextureTarget where
	marshal x = case x of
		Tex2D          -> 0x0DE1
		TexCubeMap     -> 0x8513
		TexCubeMapPosX -> 0x8515
		TexCubeMapPosY -> 0x8517
		TexCubeMapPosZ -> 0x8519
		TexCubeMapNegX -> 0x8516
		TexCubeMapNegY -> 0x8518
		TexCubeMapNegZ -> 0x851A
		Tex3D          -> 0x806F
		Tex2DArray     -> 0x8C1A


-- | ES 3.0
data Query = QueryObj { unQuery :: GLuint }

instance ServerObject Query where
	genObjects n = allocaArray n $ \arr -> do
		glGenQueries (fromIntegral n) arr
		map QueryObj <$> peekArray n arr
	bindObject _ _ = fail "bindObject for a Query does not exist."
	isObject (QueryObj x) = return . (== 1) =<< glIsQuery x
	deleteObjects xs = withArray (map unQuery xs) $ \arr ->
		glDeleteQueries (fromIntegral $ length xs) arr

-- | ES 3.0
data QueryTarget = AnySamplesPassed | AnySamplesPassedConservative
instance BindTarget	Query QueryTarget
instance Marshal QueryTarget where
	marshal AnySamplesPassed = 0x8C2F
	marshal AnySamplesPassedConservative = 0x8D6A


-- | ES 3.0
data TransformFeedback = TransformFeedbackObj { unTransformFeedback :: GLuint }

instance ServerObject TransformFeedback where
	genObjects n = allocaArray n $ \arr -> do
		glGenTransformFeedbacks (fromIntegral n) arr
		map TransformFeedbackObj <$> peekArray n arr
	bindObject target (TransformFeedbackObj x) =
		glBindTransformFeedback (marshal target) x
	isObject (TransformFeedbackObj x) =
		return . (== 1) =<< glIsTransformFeedback x
	deleteObjects xs = withArray (map unTransformFeedback xs) $ \arr ->
		glDeleteTransformFeedbacks (fromIntegral $ length xs) arr

-- | ES 3.0
data TransformFeedbackTarget = TransformFeedback
instance BindTarget	TransformFeedback TransformFeedbackTarget
instance Marshal TransformFeedbackTarget where
	marshal TransformFeedback = 0x8E22

{-
teximg2d's data::Maybe Ptr
bindzero to clear
Buffer: {ELEMENT_}ARRAY_BUFFER

TexImage2D,CopyTexImage2D,CopyTexSubImage2D,CompressedTexImage2D,CompressedTexSubImage2D
	b TEXTURE_2D, TEXTURE_CUBE_MAP_POSITIVE_{X,Y,Z},TEXTURE_CUBE_MAP_NEGATIVE_{X,Y,Z} 
TexSubImage2D,
	b* TEXTURE_CUBE_MAP_POSITIVE_{X, Y, Z},TEXTURE_CUBE_MAP_NEGATIVE_{X, Y, Z}
TexParameter{if}v?,GenerateMipmap,GetTexParameter{if}v:
	a TEXTURE_2D, TEXTURE_CUBE_MAP
BindTexture; a+b

BindFramebuffer,FramebufferTexture2D,CheckFramebufferStatus,GetFramebufferAttachmentParameteriv:
	FRAMEBUFFER

BindRenderbuffer,RenderbufferStorage,GetRenderbufferParameteriv:
	RENDERBUFFER

FramebufferRenderbuffer
-}
{-
Query: ANY_SAMPLES_PASSED{_CONSERVATIVE}
BindBuffer and rest:
	{ELEMENT_}ARRAY_BUFFER, PIXEL_{UN}PACK_BUFFER,
	COPY_{READ, WRITE}_BUFFER,
	TRANSFORM_FEEDBACK_BUFFER, UNIFORM_BUFFER
BindBufferRange
	TRANSFORM_FEEDBACK_BUFFER, UNIFORM_BUFFER
BindBufferBase
	TRANSFORM_FEEDBACK_BUFFER, UNIFORM_BUFFER

TransformFeedback: TRANSFORM_FEEDBACK
Framebuffer: FRAMEBUFFER
FramebufferRenderbuffer,FramebufferTexture2D,CheckFramebufferStatus,GetFramebufferAttachmentParameteriv:
	FRAMEBUFFER,{DRAW,READ}_FRAMEBUFFER
Renderbuffer: RENDERBUFFER
Texture: a+b+c
TexImage3D,TexStorage3D,TexSubImage3D,CopyTexSubImage3D,CompressedTexImage3D,: 
	c TEXTURE_3D, TEXTURE_2D_ARRAY
TexStorage2D:
	a TEXTURE_CUBE_MAP, TEXTURE_2D
TexImage2D,CopyTexImage2D,TexSubImage2D,CopyTexSubImage2D,CompressedTexImage2D,CompressedTexSubImage2D,CompressedTexSubImage3D:
	b TEXTURE_2D, TEXTURE_CUBE_MAP_POSITIVE_{X, Y, Z},TEXTURE_CUBE_MAP_NEGATIVE_{X, Y, Z}
TexParameter{i,f}{,v},GenerateMipmap:
	a+c TEXTURE_{2D, 3D}, TEXTURE_2D_ARRAY, TEXTURE_CUBE_MAP

3.0 TEXTURE_3D, TEXTURE_2D_ARRAY
-}

newtype Shader = ShaderObj { unShader :: GLuint } deriving Eq

data ShaderType = FragmentShader | VertexShader deriving Show
instance Marshal ShaderType where
	marshal FragmentShader = 0x8B30
	marshal VertexShader   = 0x8B31

data ShaderPName = ShaderType
                 | CompileStatus
                 | ShaderInfoLogLength
                 | ShaderSourceLength
                 | ShaderCompiler
                 -- and more on ES 3.0
instance Marshal ShaderPName where
	marshal x = case x of
		ShaderType -> 0x8B4F
		CompileStatus -> 0x8B81
		ShaderInfoLogLength -> 0x8B84
		ShaderSourceLength -> 0x8B88
		ShaderCompiler -> 0x8DFA
		-- ...

loadShader :: ShaderType -> String -> String -> IO (Either String Shader)
loadShader shaderType name code = do
	shader <- glCreateShader (marshal shaderType)
	if shader /= 0 then
		withCString code $ \src -> do
			withArray [src] $ \ptr -> do
				glShaderSource shader 1 ptr nullPtr
				glCompileShader shader
				alloca $ \pint -> do
					glGetShaderiv shader (marshal CompileStatus) pint
					compiled <- peek pint
					if compiled == 0 then do
						glGetShaderiv shader (marshal ShaderInfoLogLength) pint
						len <- peek pint
						msg <- allocaBytes (fromIntegral len) $ \buf -> do
							glGetShaderInfoLog shader len nullPtr buf
							msg <- peekCStringLen (buf,fromIntegral len)
							return $ "Could not compile " ++ show shaderType
								++ " " ++ name ++ "\n" ++ msg
						putStrLn msg
						glDeleteShader shader
						return (Left msg)
					else return . Right . ShaderObj $ shader
	else do
		showError "glCreateShader"
		return . Left $ "glCreateShader returned 0."

newtype Program = ProgramObj { unProgram :: GLuint }

data ProgramPName = DeleteStatus
                  | LinkStatus
                  | ValidateStatus
                  | ProgramInfoLogLength
                  | AttachedShaders
                  | ActiveAttributes
                  | ActiveAttributeMaxLength
                  | ActiveUniforms
                  | ActiveUniformMaxLength
                  -- and more and more on ES 3.0
instance Marshal ProgramPName where
	marshal x = case x of
		DeleteStatus -> 0x8B80
		LinkStatus -> 0x8B82
		ValidateStatus -> 0x8B83
		AttachedShaders -> 0x8B85
		ActiveAttributes -> 0x8B89
		ActiveUniformMaxLength -> 0x8B8A
		-- ......

createProgram :: [(String,String)]
              -> [(String,String)]
              -> IO (Either [String] Program)
createProgram vertexCodes fragmentCodes = do
	vx <- mapM (\(n,c)->loadShader VertexShader n c) vertexCodes
	fx <- mapM (\(n,c)->loadShader FragmentShader n c) fragmentCodes
	let right (Right _) = True; right _ = False
	let lefts = filter (not.right) (vx ++ fx)
	let lft (Left x) = x; rht (Right x) = x
	if lefts /= [] then return . Left $ map lft lefts
	else do
		let sdrs = map rht (vx ++ fx)
		program <- glCreateProgram
		if program /= 0 then do
			let attach shader = do
				glAttachShader program (unShader shader)
				showError "glAttachShader"
			mapM attach sdrs
			glLinkProgram program
			alloca $ \pint -> do
				glGetProgramiv program (marshal LinkStatus) pint
				linkStatus <- peek pint
				if linkStatus == 0 then do
					glGetProgramiv program (marshal ProgramInfoLogLength) pint
					len <- peek pint
					msg <- allocaBytes (fromIntegral len) $ \buf -> do
						glGetProgramInfoLog program len nullPtr buf
						msg <- peekCStringLen (buf,fromIntegral len)
						return $ "Could not link program:\n" ++ msg
					putStrLn msg
					glDeleteProgram program
					return (Left [msg])
				else return . Right . ProgramObj $ program
		else do
			showError "glCreateProgram"
			return (Left ["glCreateProgram returned 0."])

showError :: String -> IO ()
showError location = do
	getError >>= \err -> case err of
		NoError -> return ()
		_ -> putStrLn $ "GLError " ++ location ++ ": " ++ show err

getAttribLocation :: Program -> String -> IO Int
getAttribLocation (ProgramObj prog) name = do
	withCString name $ \str -> do
		fromIntegral <$> glGetAttribLocation prog str

getUniformLocation :: (Num a) => Program -> String -> IO a
getUniformLocation (ProgramObj prog) name = do
	withCString name $ \str -> do
		fromIntegral <$> glGetUniformLocation prog str

useProgram :: Program -> IO ()
useProgram (ProgramObj p) = glUseProgram p

deleteProgram :: Program -> IO ()
deleteProgram (ProgramObj p) = glDeleteProgram p

data DataType =
	  ByteT
	| UByteT
	| ShortT
	| UShortT
	| IntT  -- ^ Cannot used in vertexAttribPointer
	| UIntT -- ^ Cannot used in vertexAttribPointer
	| FloatT
	| FixedT
	-- and more on ES 3.0
instance Marshal DataType where
	marshal x = case x of
		ByteT -> 0x1400
		UByteT -> 0x1401
		ShortT -> 0x1402
		UShortT -> 0x1403
		IntT -> 0x1404
		UIntT -> 0x1405
		FloatT -> 0x1406
		FixedT -> 0x140C
		-- ...

vertexAttribPointer :: Int -> Int -> DataType -> Bool -> Int -> Ptr GLfloat -> IO ()
vertexAttribPointer index size typ normalized stride ptr =
	glVertexAttribPointer (fromIntegral index) (fromIntegral size)
	                      (marshal typ) (fromBool normalized)
	                      (fromIntegral stride) (unsafeCoerce ptr)

vertexAttribPointerArrayBufBound :: Int -> Int -> DataType -> Bool -> Int -> Int -> IO ()
vertexAttribPointerArrayBufBound index size typ normalized stride offset =
	glVertexAttribPointer (fromIntegral index) (fromIntegral size)
	                      (marshal typ) (fromBool normalized)
	                      (fromIntegral stride) (unsafeCoerce offset)

enableVertexAttribArray :: Int -> IO ()
enableVertexAttribArray index =
	glEnableVertexAttribArray (fromIntegral index)

data DrawMode = Points | Lines | LineLoop | LineStrip
              | Triangles | TriangleStrip | TriangleFan
instance Marshal DrawMode where
	marshal x = case x of
		Points -> 0
		Lines -> 1
		LineLoop -> 2
		LineStrip -> 3
		Triangles -> 4
		TriangleStrip -> 5
		TriangleFan -> 6

drawArrays :: DrawMode -> Int -> Int -> IO ()
drawArrays mode first count =
	glDrawArrays (marshal mode) (fromIntegral first) (fromIntegral count)

detectGLESVersion :: Int
detectGLESVersion =
	let es2 = isGLProcAvailable "glCreateShader" in
	let es3 = isGLProcAvailable "glIsQuery" in
	case (es2,es3) of
		(True, True)  -> 3
		(True, False) -> 2
		_             -> 1

class Uniform a where
	-- | set a value to an uniform variable
	uniform :: GLint -> a -> IO ()
	setUniform :: Program -> String -> a -> IO ()
	setUniform p name a =
		getUniformLocation p name >>= \loc-> uniform loc a

instance Uniform GLfloat where
	uniform = glUniform1f
instance Uniform (GLfloat,GLfloat) where
	uniform loc (x,y) = glUniform2f loc x y
instance Uniform (GLfloat,GLfloat,GLfloat) where
	uniform loc (x,y,z) = glUniform3f loc x y z
instance Uniform (GLfloat,GLfloat,GLfloat,GLfloat) where
	uniform loc (x,y,z,a) = glUniform4f loc x y z a
instance Uniform GLint where
	uniform = glUniform1i
instance Uniform (GLint,GLint) where
	uniform loc (x,y) = glUniform2i loc x y
instance Uniform (GLint,GLint,GLint) where
	uniform loc (x,y,z) = glUniform3i loc x y z
instance Uniform (GLint,GLint,GLint,GLint) where
	uniform loc (x,y,z,a) = glUniform4i loc x y z a

-- | Uniform (GLuint..) since ES 3.0
instance Uniform GLuint where
	uniform = glUniform1ui
instance Uniform (GLuint,GLuint) where
	uniform loc (x,y) = glUniform2ui loc x y
instance Uniform (GLuint,GLuint,GLuint) where
	uniform loc (x,y,z) = glUniform3ui loc x y z
instance Uniform (GLuint,GLuint,GLuint,GLuint) where
	uniform loc (x,y,z,a) = glUniform4ui loc x y z a

--class UniformVec a where
--	uniformv :: GLuint -> GLsizei -> Ptr a -> IO ()

data BufferUsage =
	  StreamDraw
	| StaticDraw
	| DynamicDraw
	| StreamRead -- ^ ES 3.0
	| StreamCopy -- ^ ES 3.0
	| StaticRead -- ^ ES 3.0
	| StaticCopy -- ^ ES 3.0
	| DynamicRead -- ^ ES 3.0
	| DynamicCopy -- ^ ES 3.0
instance Marshal BufferUsage where
	marshal x = case x of
		StreamDraw -> 0x88E0
		StreamRead -> 0x88E1
		StreamCopy -> 0x88E2
		StaticDraw -> 0x88E4
		StaticRead -> 0x88E5
		StaticCopy -> 0x88E6
		DynamicDraw -> 0x88E8
		DynamicRead -> 0x88E9
		DynamicCopy -> 0x88EA


bufferData :: BufferTarget -> Int -> Ptr a -> BufferUsage -> IO ()
bufferData target size ptr usage = do
	glBufferData (marshal target) (fromIntegral size)
	             (castPtr ptr) (marshal usage)

uniformMatrixf2 :: GLint -> Int -> Bool -> [GLfloat] -> IO ()
uniformMatrixf2 loc count transpose value =
	withArray value $ \ptr ->
		glUniformMatrix2fv loc (fromIntegral count) (fromBool transpose)
	                       ptr

uniformMatrixf3 :: GLint -> Int -> Bool -> [GLfloat] -> IO ()
uniformMatrixf3 loc count transpose value =
	withArray value $ \ptr ->
		glUniformMatrix2fv loc (fromIntegral count) (fromBool transpose)
	                       ptr

uniformMatrixf4 :: GLint -> Int -> Bool -> [GLfloat] -> IO ()
uniformMatrixf4 loc count transpose value =
	withArray value $ \ptr ->
		glUniformMatrix2fv loc (fromIntegral count) (fromBool transpose)
	                       ptr

