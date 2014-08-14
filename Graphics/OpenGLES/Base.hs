{-# LANGUAGE CPP #-}
-- | definition from gl3.h
module Graphics.OpenGLES.Base (
	GLboolean,
	GLbyte,
	GLubyte,
	GLchar,
	GLshort,
	GLushort,
	GLint,
	GLuint,
	GLfixed,
	GLsizei,
	GLenum,
	GLintptr,
	GLsizeiptr,
	GLbitfield,
	GLfloat,
	GLclampf,
	GLint64,
	GLuint64,
	GLsync,
	GLhalf,
	--isGLProcAvailable,
	glActiveTexture,
	glAttachShader,
	glBindAttribLocation,
	glBindBuffer,
	glBindFramebuffer,
	glBindRenderbuffer,
	glBindTexture,
	glBlendColor,
	glBlendEquation,
	glBlendEquationSeparate,
	glBlendFunc,
	glBlendFuncSeparate,
	glBufferData,
	glBufferSubData,
	glCheckFramebufferStatus,
	glClear,
	glClearColor,
	glClearDepthf,
	glClearStencil,
	glColorMask,
	glCompileShader,
	glCompressedTexImage2D,
	glCompressedTexSubImage2D,
	glCopyTexImage2D,
	glCopyTexSubImage2D,
	glCreateProgram,
	glCreateShader,
	glCullFace,
	glDeleteBuffers,
	glDeleteFramebuffers,
	glDeleteProgram,
	glDeleteRenderbuffers,
	glDeleteShader,
	glDeleteTextures,
	glDepthFunc,
	glDepthMask,
	glDepthRangef,
	glDetachShader,
	glDisable,
	glDisableVertexAttribArray,
	glDrawArrays,
	glDrawElements,
	glEnable,
	glEnableVertexAttribArray,
	glFinish,
	glFlush,
	glFramebufferRenderbuffer,
	glFramebufferTexture2D,
	glFrontFace,
	glGenBuffers,
	glGenerateMipmap,
	glGenFramebuffers,
	glGenRenderbuffers,
	glGenTextures,
	glGetActiveAttrib,
	glGetActiveUniform,
	glGetAttachedShaders,
	glGetAttribLocation,
	glGetBooleanv,
	glGetBufferParameteriv,
	glGetError,
	glGetFloatv,
	glGetFramebufferAttachmentParameteriv,
	glGetIntegerv,
	glGetProgramiv,
	glGetProgramInfoLog,
	glGetRenderbufferParameteriv,
	glGetShaderiv,
	glGetShaderInfoLog,
	glGetShaderPrecisionFormat,
	glGetShaderSource,
	glGetString,
	glGetTexParameterfv,
	glGetTexParameteriv,
	glGetUniformfv,
	glGetUniformiv,
	glGetUniformLocation,
	glGetVertexAttribfv,
	glGetVertexAttribiv,
	glGetVertexAttribPointerv,
	glHint,
	glIsBuffer,
	glIsEnabled,
	glIsFramebuffer,
	glIsProgram,
	glIsRenderbuffer,
	glIsShader,
	glIsTexture,
	glLineWidth,
	glLinkProgram,
	glPixelStorei,
	glPolygonOffset,
	glReadPixels,
	glReleaseShaderCompiler,
	glRenderbufferStorage,
	glSampleCoverage,
	glScissor,
	glShaderBinary,
	glShaderSource,
	glStencilFunc,
	glStencilFuncSeparate,
	glStencilMask,
	glStencilMaskSeparate,
	glStencilOp,
	glStencilOpSeparate,
	glTexImage2D,
	glTexParameterf,
	glTexParameterfv,
	glTexParameteri,
	glTexParameteriv,
	glTexSubImage2D,
	glUniform1f,
	glUniform1fv,
	glUniform1i,
	glUniform1iv,
	glUniform2f,
	glUniform2fv,
	glUniform2i,
	glUniform2iv,
	glUniform3f,
	glUniform3fv,
	glUniform3i,
	glUniform3iv,
	glUniform4f,
	glUniform4fv,
	glUniform4i,
	glUniform4iv,
	glUniformMatrix2fv,
	glUniformMatrix3fv,
	glUniformMatrix4fv,
	glUseProgram,
	glValidateProgram,
	glVertexAttrib1f,
	glVertexAttrib1fv,
	glVertexAttrib2f,
	glVertexAttrib2fv,
	glVertexAttrib3f,
	glVertexAttrib3fv,
	glVertexAttrib4f,
	glVertexAttrib4fv,
	glVertexAttribPointer,
	glViewport,

	glReadBuffer,
	glDrawRangeElements,
	glTexImage3D,
	glTexSubImage3D,
	glCopyTexSubImage3D,
	glCompressedTexImage3D,
	glCompressedTexSubImage3D,
	glGenQueries,
	glDeleteQueries,
	glIsQuery,
	glBeginQuery,
	glEndQuery,
	glGetQueryiv,
	glGetQueryObjectuiv,
	glUnmapBuffer,
	glGetBufferPointerv,
	glDrawBuffers,
	glUniformMatrix2x3fv,
	glUniformMatrix3x2fv,
	glUniformMatrix2x4fv,
	glUniformMatrix4x2fv,
	glUniformMatrix3x4fv,
	glUniformMatrix4x3fv,
	glBlitFramebuffer,
	glRenderbufferStorageMultisample,
	glFramebufferTextureLayer,
	glMapBufferRange,
	glFlushMappedBufferRange,
	glBindVertexArray,
	glDeleteVertexArrays,
	glGenVertexArrays,
	glIsVertexArray,
	glGetIntegeri_v,
	glBeginTransformFeedback,
	glEndTransformFeedback,
	glBindBufferRange,
	glBindBufferBase,
	glTransformFeedbackVaryings,
	glGetTransformFeedbackVarying,
	glVertexAttribIPointer,
	glGetVertexAttribIiv,
	glGetVertexAttribIuiv,
	glVertexAttribI4i,
	glVertexAttribI4ui,
	glVertexAttribI4iv,
	glVertexAttribI4uiv,
	glGetUniformuiv,
	glGetFragDataLocation,
	glUniform1ui,
	glUniform2ui,
	glUniform3ui,
	glUniform4ui,
	glUniform1uiv,
	glUniform2uiv,
	glUniform3uiv,
	glUniform4uiv,
	glClearBufferiv,
	glClearBufferuiv,
	glClearBufferfv,
	glClearBufferfi,
	glGetStringi,
	glCopyBufferSubData,
	glGetUniformIndices,
	glGetActiveUniformsiv,
	glGetUniformBlockIndex,
	glGetActiveUniformBlockiv,
	glGetActiveUniformBlockName,
	glUniformBlockBinding,
	glDrawArraysInstanced,
	glDrawElementsInstanced,
	glFenceSync,
	glIsSync,
	glDeleteSync,
	glClientWaitSync,
	glWaitSync,
	glGetInteger64v,
	glGetSynciv,
	glGetInteger64i_v,
	glGetBufferParameteri64v,
	glGenSamplers,
	glDeleteSamplers,
	glIsSampler,
	glBindSampler,
	glSamplerParameteri,
	glSamplerParameteriv,
	glSamplerParameterf,
	glSamplerParameterfv,
	glGetSamplerParameteriv,
	glGetSamplerParameterfv,
	glVertexAttribDivisor,
	glBindTransformFeedback,
	glDeleteTransformFeedbacks,
	glGenTransformFeedbacks,
	glIsTransformFeedback,
	glPauseTransformFeedback,
	glResumeTransformFeedback,
	glGetProgramBinary,
	glProgramBinary,
	glProgramParameteri,
	glInvalidateFramebuffer,
	glInvalidateSubFramebuffer,
	glTexStorage2D,
	glTexStorage3D,
	glGetInternalformativ,

	glDrawTexiOES,
	glMultiDrawArraysEXT,
	glMultiDrawElementsEXT
	) where
import Foreign
import Foreign.C.String
import Graphics.EGL (eglGetProcAddress)
--XXX unify Wrappers by args
-- * Basic Types
-- ** OpenGL ES 2.0

-- | 1bit boolean
type GLboolean = Word8

-- | 8bit signed two\'s complement binary integer
type GLbyte = Int8

-- | 8bit unsigned binary integer
type GLubyte = Word8

-- | (Unused) 8bit characters making up strings
type GLchar = Int8

-- | 16bit signed two\'s complement binary integer
type GLshort = Int16

-- | 16bit unsigned binary integer
type GLushort = Word16

-- | 32bit signed two\'s complement binary integer
type GLint = Int32

-- | 32bit unsigned binary integer
type GLuint = Word32

-- | 32bit signed two\'s complement 16.16 scaled integer
type GLfixed = Int32

-- | 32bit non-negative binary integer size
type GLsizei = Word32

-- | 32bit enumerated binary integer value
type GLenum = Word32

-- | Pointer-sized signed two\'s complement binary integer
type GLintptr = Int

-- | Pointer-sized non-negative binary integer size
type GLsizeiptr = Word

-- | 32bit bit field
type GLbitfield = Word32

-- | 32bit floating-point value
type GLfloat = Float

-- | 32bit floating-point value clamped to [0,1]
type GLclampf = Float

-- ** OpenGL ES 3.0

-- | 64bit signed two\'s complement binary integer
type GLint64 = Int64

-- | 64bit unsigned binary integer
type GLuint64 = Word64

-- | Pointer-sized sync object handle
type GLsync = Ptr ()

-- | 16bit half-precision floating-point value encoded in an unsigned scalar
type GLhalf = Word16


-- * Wrappers

{-isGLProcAvailable :: String -> Bool
isGLProcAvailable name = let
	proc = castFunPtrToPtr (eglGetProcAddress name)
	undef = castFunPtrToPtr (eglGetProcAddress "glUndefined")
	in abs (proc `minusPtr` undef) > 0x10000
	-- eglGetProcAddress may return an useless pointer when
	-- given name starts with "gl" but not implemented.
-}
-- Declere must-have functions
#define GLES2(_procname, _typ) \
foreign import ccall unsafe "GLES2/gl2.h" _procname :: _typ; \
--{- avoid inlining to save size # INLINE _procname #-} \

-- Work around for a runtime link error
#define GL_EXT(_procname, _typ) \
foreign import ccall unsafe "dynamic" unwrap_/**/_procname :: FunPtr (_typ) -> _typ; \
_procname :: _typ; \
_procname = unwrap_/**/_procname (eglGetProcAddress "_procname") ; \
{-# NOINLINE _procname #-} \
-- foreign import ccall unsafe "dynamic"
--   unwrap_glActiveTexture :: FunPtr (GLenum -> IO ()) -> GLenum -> IO ();
-- glActiveTexture :: GLenum -> IO ();
-- glActiveTexture = unwrap_glActiveTexture (eglGetProcAddress "glActiveTexture");


-- ** OpenGL ES 2.0

GLES2(glActiveTexture, GLenum -> IO ())
GLES2(glAttachShader, GLuint -> GLuint -> IO ())
GLES2(glBindAttribLocation, GLuint -> GLuint -> CString -> IO ())
GLES2(glBindBuffer, GLenum -> GLuint -> IO ())
GLES2(glBindFramebuffer, GLenum -> GLuint -> IO ())
GLES2(glBindRenderbuffer, GLenum -> GLuint -> IO ())
GLES2(glBindTexture, GLenum -> GLuint -> IO ())
GLES2(glBlendColor, GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ())
GLES2(glBlendEquation, GLenum -> IO ())
GLES2(glBlendEquationSeparate, GLenum -> GLenum -> IO ())
GLES2(glBlendFunc, GLenum -> GLenum -> IO ())
GLES2(glBlendFuncSeparate, GLenum -> GLenum -> GLenum -> GLenum -> IO ())
GLES2(glBufferData, GLenum -> GLsizeiptr -> Ptr () -> GLenum -> IO ())
GLES2(glBufferSubData, GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> IO ())
GLES2(glCheckFramebufferStatus, GLenum -> IO GLenum)
GLES2(glClear, GLbitfield -> IO ())
GLES2(glClearColor, GLclampf -> GLclampf -> GLclampf -> GLclampf -> IO ())
GLES2(glClearDepthf, GLclampf -> IO ())
GLES2(glClearStencil, GLint -> IO ())
GLES2(glColorMask, GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
GLES2(glCompileShader, GLuint -> IO ())
GLES2(glCompressedTexImage2D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
GLES2(glCompressedTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
GLES2(glCopyTexImage2D, GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
GLES2(glCopyTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GLES2(glCreateProgram, IO GLuint)
GLES2(glCreateShader, GLenum -> IO GLuint)
GLES2(glCullFace, GLenum -> IO ())
GLES2(glDeleteBuffers, GLsizei -> Ptr GLuint -> IO ())
GLES2(glDeleteFramebuffers, GLsizei -> Ptr GLuint -> IO ())
GLES2(glDeleteProgram, GLuint -> IO ())
GLES2(glDeleteRenderbuffers, GLsizei -> Ptr GLuint -> IO ())
GLES2(glDeleteShader, GLuint -> IO ())
GLES2(glDeleteTextures, GLsizei -> Ptr GLuint -> IO ())
GLES2(glDepthFunc, GLenum -> IO ())
GLES2(glDepthMask, GLboolean -> IO ())
GLES2(glDepthRangef, GLclampf -> GLclampf -> IO ())
GLES2(glDetachShader, GLuint -> GLuint -> IO ())
GLES2(glDisable, GLenum -> IO ())
GLES2(glDisableVertexAttribArray, GLuint -> IO ())
GLES2(glDrawArrays, GLenum -> GLint -> GLsizei -> IO ())
GLES2(glDrawElements, GLenum -> GLsizei -> GLenum -> Ptr () -> IO ())
GLES2(glEnable, GLenum -> IO ())
GLES2(glEnableVertexAttribArray, GLuint -> IO ())
GLES2(glFinish, IO ())
GLES2(glFlush, IO ())
GLES2(glFramebufferRenderbuffer, GLenum -> GLenum -> GLenum -> GLuint -> IO ())
GLES2(glFramebufferTexture2D, GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
GLES2(glFrontFace, GLenum -> IO ())
GLES2(glGenBuffers, GLsizei -> Ptr GLuint -> IO ())
GLES2(glGenerateMipmap, GLenum -> IO ())
GLES2(glGenFramebuffers, GLsizei -> Ptr GLuint -> IO ())
GLES2(glGenRenderbuffers, GLsizei -> Ptr GLuint -> IO ())
GLES2(glGenTextures, GLsizei -> Ptr GLuint -> IO ())
GLES2(glGetActiveAttrib, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> CString -> IO ())
GLES2(glGetActiveUniform, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> CString -> IO ())
GLES2(glGetAttachedShaders, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())
GLES2(glGetAttribLocation, GLuint -> CString -> IO GLint)
GLES2(glGetBooleanv, GLenum -> Ptr GLboolean -> IO ())
GLES2(glGetBufferParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GLES2(glGetError, IO GLenum)
GLES2(glGetFloatv, GLenum -> Ptr GLfloat -> IO ())
GLES2(glGetFramebufferAttachmentParameteriv, GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
GLES2(glGetIntegerv, GLenum -> Ptr GLint -> IO ())
GLES2(glGetProgramiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GLES2(glGetProgramInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> CString -> IO ())
GLES2(glGetRenderbufferParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GLES2(glGetShaderiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GLES2(glGetShaderInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> CString -> IO ())
GLES2(glGetShaderPrecisionFormat, GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ())
GLES2(glGetShaderSource, GLuint -> GLsizei -> Ptr GLsizei -> CString -> IO ())
GLES2(glGetString, GLenum -> IO CString)
GLES2(glGetTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> IO ())
GLES2(glGetTexParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GLES2(glGetUniformfv, GLuint -> GLint -> Ptr GLfloat -> IO ())
GLES2(glGetUniformiv, GLuint -> GLint -> Ptr GLint -> IO ())
GLES2(glGetUniformLocation, GLuint -> CString -> IO GLint)
GLES2(glGetVertexAttribfv, GLuint -> GLenum -> Ptr GLfloat -> IO ())
GLES2(glGetVertexAttribiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GLES2(glGetVertexAttribPointerv, GLuint -> GLenum -> Ptr (Ptr ()) -> IO ())
GLES2(glHint, GLenum -> GLenum -> IO ())
GLES2(glIsBuffer, GLuint -> IO GLboolean)
GLES2(glIsEnabled, GLenum -> IO GLboolean)
GLES2(glIsFramebuffer, GLuint -> IO GLboolean)
GLES2(glIsProgram, GLuint -> IO GLboolean)
GLES2(glIsRenderbuffer, GLuint -> IO GLboolean)
GLES2(glIsShader, GLuint -> IO GLboolean)
GLES2(glIsTexture, GLuint -> IO GLboolean)
GLES2(glLineWidth, GLfloat -> IO ())
GLES2(glLinkProgram, GLuint -> IO ())
GLES2(glPixelStorei, GLenum -> GLint -> IO ())
GLES2(glPolygonOffset, GLfloat -> GLfloat -> IO ())
GLES2(glReadPixels, GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
GLES2(glReleaseShaderCompiler, IO ())
GLES2(glRenderbufferStorage, GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
GLES2(glSampleCoverage, GLclampf -> GLboolean -> IO ())
GLES2(glScissor, GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GLES2(glShaderBinary, GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> IO ())
GLES2(glShaderSource, GLuint -> GLsizei -> Ptr CString -> Ptr GLint -> IO ())
GLES2(glStencilFunc, GLenum -> GLint -> GLuint -> IO ())
GLES2(glStencilFuncSeparate, GLenum -> GLenum -> GLint -> GLuint -> IO ())
GLES2(glStencilMask, GLuint -> IO ())
GLES2(glStencilMaskSeparate, GLenum -> GLuint -> IO ())
GLES2(glStencilOp, GLenum -> GLenum -> GLenum -> IO ())
GLES2(glStencilOpSeparate, GLenum -> GLenum -> GLenum -> GLenum -> IO ())
GLES2(glTexImage2D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> IO ())
GLES2(glTexParameterf, GLenum -> GLenum -> GLfloat -> IO ())
GLES2(glTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> IO ())
GLES2(glTexParameteri, GLenum -> GLenum -> GLint -> IO ())
GLES2(glTexParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GLES2(glTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
GLES2(glUniform1f, GLint -> GLfloat -> IO ())
GLES2(glUniform1fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GLES2(glUniform1i, GLint -> GLint -> IO ())
GLES2(glUniform1iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GLES2(glUniform2f, GLint -> GLfloat -> GLfloat -> IO ())
GLES2(glUniform2fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GLES2(glUniform2i, GLint -> GLint -> GLint -> IO ())
GLES2(glUniform2iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GLES2(glUniform3f, GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
GLES2(glUniform3fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GLES2(glUniform3i, GLint -> GLint -> GLint -> GLint -> IO ())
GLES2(glUniform3iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GLES2(glUniform4f, GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
GLES2(glUniform4fv, GLint -> GLsizei -> Ptr GLfloat -> IO ())
GLES2(glUniform4i, GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
GLES2(glUniform4iv, GLint -> GLsizei -> Ptr GLint -> IO ())
GLES2(glUniformMatrix2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GLES2(glUniformMatrix3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GLES2(glUniformMatrix4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GLES2(glUseProgram, GLuint -> IO ())
GLES2(glValidateProgram, GLuint -> IO ())
GLES2(glVertexAttrib1f, GLuint -> GLfloat -> IO ())
GLES2(glVertexAttrib1fv, GLuint -> Ptr GLfloat -> IO ())
GLES2(glVertexAttrib2f, GLuint -> GLfloat -> GLfloat -> IO ())
GLES2(glVertexAttrib2fv, GLuint -> Ptr GLfloat -> IO ())
GLES2(glVertexAttrib3f, GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
GLES2(glVertexAttrib3fv, GLuint -> Ptr GLfloat -> IO ())
GLES2(glVertexAttrib4f, GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
GLES2(glVertexAttrib4fv, GLuint -> Ptr GLfloat -> IO ())
GLES2(glVertexAttribPointer, GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> IO ())
GLES2(glViewport, GLint -> GLint -> GLsizei -> GLsizei -> IO ())

-- ** OpenGL ES 3.0

GL_EXT(glReadBuffer, GLenum -> IO ())
GL_EXT(glDrawRangeElements, GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr (()) -> IO ())
GL_EXT(glTexImage3D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr (()) -> IO ())
GL_EXT(glTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> IO ())
GL_EXT(glCopyTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GL_EXT(glCompressedTexImage3D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> IO ())
GL_EXT(glCompressedTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> IO ())
GL_EXT(glGenQueries, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glDeleteQueries, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glIsQuery, GLuint -> IO GLboolean)
GL_EXT(glBeginQuery, GLenum -> GLuint -> IO ())
GL_EXT(glEndQuery, GLenum -> IO ())
GL_EXT(glGetQueryiv, GLenum -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetQueryObjectuiv, GLuint -> GLenum -> Ptr GLuint -> IO ())
GL_EXT(glUnmapBuffer, GLenum -> IO GLboolean)
GL_EXT(glGetBufferPointerv, GLenum -> GLenum -> Ptr (Ptr ()) -> IO ())
GL_EXT(glDrawBuffers, GLsizei -> Ptr GLenum -> IO ())
GL_EXT(glUniformMatrix2x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glUniformMatrix3x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glUniformMatrix2x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glUniformMatrix4x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glUniformMatrix3x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glUniformMatrix4x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glBlitFramebuffer, GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
GL_EXT(glRenderbufferStorageMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
GL_EXT(glFramebufferTextureLayer, GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
GL_EXT(glMapBufferRange, GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr ()))
GL_EXT(glFlushMappedBufferRange, GLenum -> GLintptr -> GLsizeiptr -> IO ())
GL_EXT(glBindVertexArray, GLuint -> IO ())
GL_EXT(glDeleteVertexArrays, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glGenVertexArrays, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glIsVertexArray, GLuint -> IO GLboolean)
GL_EXT(glGetIntegeri_v, GLenum -> GLuint -> Ptr GLint -> IO ())
GL_EXT(glBeginTransformFeedback, GLenum -> IO ())
GL_EXT(glEndTransformFeedback, IO ())
GL_EXT(glBindBufferRange, GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
GL_EXT(glBindBufferBase, GLenum -> GLuint -> GLuint -> IO ())
GL_EXT(glTransformFeedbackVaryings, GLuint -> GLsizei -> Ptr CString -> GLenum -> IO ())
GL_EXT(glGetTransformFeedbackVarying, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> CString -> IO ())
GL_EXT(glVertexAttribIPointer, GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> IO ())
GL_EXT(glGetVertexAttribIiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetVertexAttribIuiv, GLuint -> GLenum -> Ptr GLuint -> IO ())
GL_EXT(glVertexAttribI4i, GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
GL_EXT(glVertexAttribI4ui, GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
GL_EXT(glVertexAttribI4iv, GLuint -> Ptr GLint -> IO ())
GL_EXT(glVertexAttribI4uiv, GLuint -> Ptr GLuint -> IO ())
GL_EXT(glGetUniformuiv, GLuint -> GLint -> Ptr GLuint -> IO ())
GL_EXT(glGetFragDataLocation, GLuint -> CString-> IO GLint)
GL_EXT(glUniform1ui, GLint -> GLuint -> IO ())
GL_EXT(glUniform2ui, GLint -> GLuint -> GLuint -> IO ())
GL_EXT(glUniform3ui, GLint -> GLuint -> GLuint -> GLuint -> IO ())
GL_EXT(glUniform4ui, GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
GL_EXT(glUniform1uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glUniform2uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glUniform3uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glUniform4uiv, GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glClearBufferiv, GLenum -> GLint -> Ptr GLint -> IO ())
GL_EXT(glClearBufferuiv, GLenum -> GLint -> Ptr GLuint -> IO ())
GL_EXT(glClearBufferfv, GLenum -> GLint -> Ptr GLfloat -> IO ())
GL_EXT(glClearBufferfi, GLenum -> GLint -> GLfloat -> GLint -> IO ())
GL_EXT(glGetStringi, GLenum -> GLuint -> IO CString)
GL_EXT(glCopyBufferSubData, GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
GL_EXT(glGetUniformIndices, GLuint -> GLsizei -> Ptr CString -> Ptr GLuint -> IO ())
GL_EXT(glGetActiveUniformsiv, GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetUniformBlockIndex, GLuint -> CString -> IO GLuint)
GL_EXT(glGetActiveUniformBlockiv, GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetActiveUniformBlockName, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> IO ())
GL_EXT(glUniformBlockBinding, GLuint -> GLuint -> GLuint -> IO ())
GL_EXT(glDrawArraysInstanced, GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
GL_EXT(glDrawElementsInstanced, GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> IO ())
GL_EXT(glFenceSync, GLenum -> GLbitfield -> IO GLsync)
GL_EXT(glIsSync, GLsync -> IO GLboolean)
GL_EXT(glDeleteSync, GLsync -> IO ())
GL_EXT(glClientWaitSync, GLsync -> GLbitfield -> GLuint64 -> IO GLenum)
GL_EXT(glWaitSync, GLsync -> GLbitfield -> GLuint64 -> IO ())
GL_EXT(glGetInteger64v, GLenum -> Ptr GLint64 -> IO ())
GL_EXT(glGetSynciv, GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
GL_EXT(glGetInteger64i_v, GLenum -> GLuint -> Ptr GLint64 -> IO ())
GL_EXT(glGetBufferParameteri64v, GLenum -> GLenum -> Ptr GLint64 -> IO ())
GL_EXT(glGenSamplers, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glDeleteSamplers, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glIsSampler, GLuint -> IO GLboolean)
GL_EXT(glBindSampler, GLuint -> GLuint -> IO ())
GL_EXT(glSamplerParameteri, GLuint -> GLenum -> GLint -> IO ())
GL_EXT(glSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glSamplerParameterf, GLuint -> GLenum -> GLfloat -> IO ())
GL_EXT(glSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> IO ())
GL_EXT(glGetSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> IO ())
GL_EXT(glVertexAttribDivisor, GLuint -> GLuint -> IO ())
GL_EXT(glBindTransformFeedback, GLenum -> GLuint -> IO ())
GL_EXT(glDeleteTransformFeedbacks, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glGenTransformFeedbacks, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glIsTransformFeedback, GLuint -> IO GLboolean)
GL_EXT(glPauseTransformFeedback, IO ())
GL_EXT(glResumeTransformFeedback, IO ())
GL_EXT(glGetProgramBinary, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> IO ())
GL_EXT(glProgramBinary, GLuint -> GLenum -> Ptr () -> GLsizei -> IO ())
GL_EXT(glProgramParameteri, GLuint -> GLenum -> GLint -> IO ())
GL_EXT(glInvalidateFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> IO ())
GL_EXT(glInvalidateSubFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
GL_EXT(glTexStorage2D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
GL_EXT(glTexStorage3D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
GL_EXT(glGetInternalformativ, GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ())

-- ** Extensions
GL_EXT(glDrawTexiOES, GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
GL_EXT(glMultiDrawArraysEXT, GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
GL_EXT(glMultiDrawElementsEXT, GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> IO ())

-- ** OpenGL ES 3.1
GL_EXT(glDispatchCompute, GLuint -> GLuint -> GLuint -> IO ())
GL_EXT(glDispatchComputeIndirect, GLintptr -> IO ())
GL_EXT(glDrawArraysIndirect, GLenum -> Ptr () -> IO ())
GL_EXT(glDrawElementsIndirect, GLenum -> GLenum -> Ptr () -> IO ())
GL_EXT(glFramebufferParameteri, GLenum -> GLenum -> GLint -> IO ())
GL_EXT(glGetFramebufferParameteriv, GLenum -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetProgramInterfaceiv, GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetProgramResourceIndex, GLuint -> GLenum -> Ptr GLchar -> IO ())
GL_EXT(glGetProgramResourceName, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
GL_EXT(glGetProgramResourceiv, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
GL_EXT(glGetProgramResourceLocation, GLuint -> GLenum -> Ptr GLchar -> IO ())
GL_EXT(glUseProgramStages, GLuint -> GLbitfield -> GLuint -> IO ())
GL_EXT(glActiveShaderProgram, GLuint -> GLuint -> IO ())
GL_EXT(glCreateShaderProgramv, GLenum -> GLsizei -> Ptr CString -> IO ())
GL_EXT(glBindProgramPipeline, GLuint -> IO ())
GL_EXT(glDeleteProgramPipelines, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glGenProgramPipelines, GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glIsProgramPipeline, GLuint -> IO ())
GL_EXT(glGetProgramPipelineiv, GLuint -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glProgramUniform1i, GLuint -> GLint -> GLint -> IO ())
GL_EXT(glProgramUniform2i, GLuint -> GLint -> GLint -> GLint -> IO ())
GL_EXT(glProgramUniform3i, GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
GL_EXT(glProgramUniform4i, GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
GL_EXT(glProgramUniform1ui, GLuint -> GLint -> GLuint -> IO ())
GL_EXT(glProgramUniform2ui, GLuint -> GLint -> GLuint -> GLuint -> IO ())
GL_EXT(glProgramUniform3ui, GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ())
GL_EXT(glProgramUniform4ui, GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
GL_EXT(glProgramUniform1f, GLuint -> GLint -> GLfloat -> IO ())
GL_EXT(glProgramUniform2f, GLuint -> GLint -> GLfloat -> GLfloat -> IO ())
GL_EXT(glProgramUniform3f, GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
GL_EXT(glProgramUniform4f, GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
GL_EXT(glProgramUniform1iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
GL_EXT(glProgramUniform2iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
GL_EXT(glProgramUniform3iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
GL_EXT(glProgramUniform4iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
GL_EXT(glProgramUniform1uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glProgramUniform2uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glProgramUniform3uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glProgramUniform4uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
GL_EXT(glProgramUniform1fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniform2fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniform3fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniform4fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix2x3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix3x2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix2x4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix4x2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix3x4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glProgramUniformMatrix4x3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
GL_EXT(glValidateProgramPipeline, GLuint -> IO ())
GL_EXT(glGetProgramPipelineInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
GL_EXT(glBindImageTexture, GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> IO ())
GL_EXT(glGetBooleani_v, GLenum -> GLuint -> Ptr GLboolean -> IO ())
GL_EXT(glMemoryBarrier, GLbitfield -> IO ())
GL_EXT(glMemoryBarrierByRegion, GLbitfield -> IO ())
GL_EXT(glTexStorage2DMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
GL_EXT(glGetMultisamplefv, GLenum -> GLuint -> Ptr GLfloat -> IO ())
GL_EXT(glSampleMaski, GLuint -> GLbitfield -> IO ())
GL_EXT(glGetTexLevelParameteriv, GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
GL_EXT(glGetTexLevelParameterfv, GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
GL_EXT(glBindVertexBuffer, GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
GL_EXT(glVertexAttribFormat, GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
GL_EXT(glVertexAttribIFormat, GLuint -> GLint -> GLenum -> GLuint -> IO ())
GL_EXT(glVertexAttribBinding, GLuint -> GLuint -> IO ())
GL_EXT(glVertexBindingDivisor, GLuint -> GLuint -> IO ())
