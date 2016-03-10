{-# LANGUAGE CPP #-}
-- | definition from gl3.h
module Graphics.OpenGLES.Base (
	-- * Basic Types
	GL,
	GLName,
	-- ** OpenGL ES 2.0
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
	-- ** OpenGL ES 3.0
	GLint64,
	GLuint64,
	GLsync,
	GLhalf,

	-- * Bindings
	--isGLProcAvailable,

	-- ** OpenGL ES 2.0
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

	-- ** OpenGL ES 3.0
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

	-- ** Extensions
	glGenVertexArraysOES,
	glBindVertexArrayOES,
	glDeleteVertexArraysOES,
	glIsVertexArrayOES,

	glDrawTexiOES,
	
	glMultiDrawArraysEXT,
	glMultiDrawElementsEXT,
	glDrawRangeElementsEXT,

	-- ** OpenGL ES 3.1
	glDispatchCompute,
	glDispatchComputeIndirect,
	glDrawArraysIndirect,
	glDrawElementsIndirect,
	glFramebufferParameteri,
	glGetFramebufferParameteriv,
	glGetProgramInterfaceiv,
	glGetProgramResourceIndex,
	glGetProgramResourceName,
	glGetProgramResourceiv,
	glGetProgramResourceLocation,
	glUseProgramStages,
	glActiveShaderProgram,
	glCreateShaderProgramv,
	glBindProgramPipeline,
	glDeleteProgramPipelines,
	glGenProgramPipelines,
	glIsProgramPipeline,
	glGetProgramPipelineiv,
	glProgramUniform1i,
	glProgramUniform2i,
	glProgramUniform3i,
	glProgramUniform4i,
	glProgramUniform1ui,
	glProgramUniform2ui,
	glProgramUniform3ui,
	glProgramUniform4ui,
	glProgramUniform1f,
	glProgramUniform2f,
	glProgramUniform3f,
	glProgramUniform4f,
	glProgramUniform1iv,
	glProgramUniform2iv,
	glProgramUniform3iv,
	glProgramUniform4iv,
	glProgramUniform1uiv,
	glProgramUniform2uiv,
	glProgramUniform3uiv,
	glProgramUniform4uiv,
	glProgramUniform1fv,
	glProgramUniform2fv,
	glProgramUniform3fv,
	glProgramUniform4fv,
	glProgramUniformMatrix2fv,
	glProgramUniformMatrix3fv,
	glProgramUniformMatrix4fv,
	glProgramUniformMatrix2x3fv,
	glProgramUniformMatrix3x2fv,
	glProgramUniformMatrix2x4fv,
	glProgramUniformMatrix4x2fv,
	glProgramUniformMatrix3x4fv,
	glProgramUniformMatrix4x3fv,
	glValidateProgramPipeline,
	glGetProgramPipelineInfoLog,
	glBindImageTexture,
	glGetBooleani_v,
	glMemoryBarrier,
	glMemoryBarrierByRegion,
	glTexStorage2DMultisample,
	glGetMultisamplefv,
	glSampleMaski,
	glGetTexLevelParameteriv,
	glGetTexLevelParameterfv,
	glBindVertexBuffer,
	glVertexAttribFormat,
	glVertexAttribIFormat,
	glVertexAttribBinding,
	glVertexBindingDivisor
	) where

import Foreign
import Foreign.C.String
import Graphics.OpenGLES.Base.Proc (glGetProcAddress)


-- * Basic Types

-- | IO actions run in GL thread.
type GL = IO

-- | Name of a shader, program, and variable.
type GLName = String

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
type GLsizei = Int32

-- | 32bit enumerated binary integer value
type GLenum = Word32

-- | Pointer-sized signed two\'s complement binary integer
type GLintptr = Int

-- | Pointer-sized non-negative binary integer size
type GLsizeiptr = Int

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


-- * Bindings

{-
-- | eglGetProcAddress may return an incremental useless pointer
-- if given name starts with \"gl\" but not implemented.
isGLProcAvailable :: String -> Bool
isGLProcAvailable name =
	abs (proc `minusPtr` undef) > 0x10000
	where
		proc = castFunPtrToPtr (eglGetProcAddress name)
		undef = castFunPtrToPtr (eglGetProcAddress "glUndefined")
-}

-- Declere must-have functions
#define STATIC(_procname, _typ) \
foreign import ccall unsafe "_procname" _procname :: _typ; \

-- Workaround for runtime link errors
-- TBD unify wrappers by types
#define DYNAMIC(_procname, _typ) \
foreign import ccall unsafe "dynamic" unwrap_/**/_procname :: FunPtr (_typ) -> _typ; \
_procname :: _typ; \
_procname = unwrap_/**/_procname (glGetProcAddress "_procname"); \

-- foreign import ccall unsafe "dynamic"
--   unwrap_glActiveTexture :: FunPtr (GLenum -> GL ()) -> GLenum -> GL ();
-- glActiveTexture :: GLenum -> GL ();
-- glActiveTexture = unwrap_glActiveTexture (glGetProcAddress "glActiveTexture");

#if defined(STATIC_ES2)
#define GLES2 STATIC
#else
#define GLES2 DYNAMIC
#endif

#if defined(STATIC_ES3)
#define GLES3 STATIC
#else
#define GLES3 DYNAMIC
#endif

#define GLES31 DYNAMIC

#define GL_EXT DYNAMIC


-- ** OpenGL ES 2.0

GLES2(glActiveTexture, GLenum -> GL ())
GLES2(glAttachShader, GLuint -> GLuint -> GL ())
GLES2(glBindAttribLocation, GLuint -> GLuint -> CString -> GL ())
GLES2(glBindBuffer, GLenum -> GLuint -> GL ())
GLES2(glBindFramebuffer, GLenum -> GLuint -> GL ())
GLES2(glBindRenderbuffer, GLenum -> GLuint -> GL ())
GLES2(glBindTexture, GLenum -> GLuint -> GL ())
GLES2(glBlendColor, GLclampf -> GLclampf -> GLclampf -> GLclampf -> GL ())
GLES2(glBlendEquation, GLenum -> GL ())
GLES2(glBlendEquationSeparate, GLenum -> GLenum -> GL ())
GLES2(glBlendFunc, GLenum -> GLenum -> GL ())
GLES2(glBlendFuncSeparate, GLenum -> GLenum -> GLenum -> GLenum -> GL ())
GLES2(glBufferData, GLenum -> GLsizeiptr -> Ptr () -> GLenum -> GL ())
GLES2(glBufferSubData, GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> GL ())
GLES2(glCheckFramebufferStatus, GLenum -> GL GLenum)
GLES2(glClear, GLbitfield -> GL ())
GLES2(glClearColor, GLclampf -> GLclampf -> GLclampf -> GLclampf -> GL ())
GLES2(glClearDepthf, GLclampf -> GL ())
GLES2(glClearStencil, GLint -> GL ())
GLES2(glColorMask, GLboolean -> GLboolean -> GLboolean -> GLboolean -> GL ())
GLES2(glCompileShader, GLuint -> GL ())
--GLES2(glCompressedTexImage2D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> GL ())
GLES2(glCompressedTexImage2D, GLenum -> GLint -> GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> CString -> GL ())
GLES2(glCompressedTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLES2(glCopyTexImage2D, GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GL ())
GLES2(glCopyTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLES2(glCreateProgram, GL GLuint)
GLES2(glCreateShader, GLenum -> GL GLuint)
GLES2(glCullFace, GLenum -> GL ())
GLES2(glDeleteBuffers, GLsizei -> Ptr GLuint -> GL ())
GLES2(glDeleteFramebuffers, GLsizei -> Ptr GLuint -> GL ())
GLES2(glDeleteProgram, GLuint -> GL ())
GLES2(glDeleteRenderbuffers, GLsizei -> Ptr GLuint -> GL ())
GLES2(glDeleteShader, GLuint -> GL ())
GLES2(glDeleteTextures, GLsizei -> Ptr GLuint -> GL ())
GLES2(glDepthFunc, GLenum -> GL ())
GLES2(glDepthMask, GLboolean -> GL ())
GLES2(glDepthRangef, GLclampf -> GLclampf -> GL ())
GLES2(glDetachShader, GLuint -> GLuint -> GL ())
GLES2(glDisable, GLenum -> GL ())
GLES2(glDisableVertexAttribArray, GLuint -> GL ())
GLES2(glDrawArrays, GLenum -> GLint -> GLsizei -> GL ())
GLES2(glDrawElements, GLenum -> GLsizei -> GLenum -> Ptr () -> GL ())
GLES2(glEnable, GLenum -> GL ())
GLES2(glEnableVertexAttribArray, GLuint -> GL ())
GLES2(glFinish, GL ())
GLES2(glFlush, GL ())
GLES2(glFramebufferRenderbuffer, GLenum -> GLenum -> GLenum -> GLuint -> GL ())
GLES2(glFramebufferTexture2D, GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GL ())
GLES2(glFrontFace, GLenum -> GL ())
GLES2(glGenBuffers, GLsizei -> Ptr GLuint -> GL ())
GLES2(glGenerateMipmap, GLenum -> GL ())
GLES2(glGenFramebuffers, GLsizei -> Ptr GLuint -> GL ())
GLES2(glGenRenderbuffers, GLsizei -> Ptr GLuint -> GL ())
GLES2(glGenTextures, GLsizei -> Ptr GLuint -> GL ())
GLES2(glGetActiveAttrib, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> CString -> GL ())
GLES2(glGetActiveUniform, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> CString -> GL ())
GLES2(glGetAttachedShaders, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> GL ())
GLES2(glGetAttribLocation, GLuint -> CString -> GL GLint)
GLES2(glGetBooleanv, GLenum -> Ptr GLboolean -> GL ())
GLES2(glGetBufferParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLES2(glGetError, GL GLenum)
GLES2(glGetFloatv, GLenum -> Ptr GLfloat -> GL ())
GLES2(glGetFramebufferAttachmentParameteriv, GLenum -> GLenum -> GLenum -> Ptr GLint -> GL ())
GLES2(glGetIntegerv, GLenum -> Ptr GLint -> GL ())
GLES2(glGetProgramiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLES2(glGetProgramInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLES2(glGetRenderbufferParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLES2(glGetShaderiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLES2(glGetShaderInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLES2(glGetShaderPrecisionFormat, GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> GL ())
GLES2(glGetShaderSource, GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLES2(glGetString, GLenum -> GL CString)
GLES2(glGetTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> GL ())
GLES2(glGetTexParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLES2(glGetUniformfv, GLuint -> GLint -> Ptr GLfloat -> GL ())
GLES2(glGetUniformiv, GLuint -> GLint -> Ptr GLint -> GL ())
GLES2(glGetUniformLocation, GLuint -> CString -> GL GLint)
GLES2(glGetVertexAttribfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLES2(glGetVertexAttribiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLES2(glGetVertexAttribPointerv, GLuint -> GLenum -> Ptr (Ptr ()) -> GL ())
GLES2(glHint, GLenum -> GLenum -> GL ())
GLES2(glIsBuffer, GLuint -> GL GLboolean)
GLES2(glIsEnabled, GLenum -> GL GLboolean)
GLES2(glIsFramebuffer, GLuint -> GL GLboolean)
GLES2(glIsProgram, GLuint -> GL GLboolean)
GLES2(glIsRenderbuffer, GLuint -> GL GLboolean)
GLES2(glIsShader, GLuint -> GL GLboolean)
GLES2(glIsTexture, GLuint -> GL GLboolean)
GLES2(glLineWidth, GLfloat -> GL ())
GLES2(glLinkProgram, GLuint -> GL ())
GLES2(glPixelStorei, GLenum -> GLint -> GL ())
GLES2(glPolygonOffset, GLfloat -> GLfloat -> GL ())
GLES2(glReadPixels, GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLES2(glReleaseShaderCompiler, GL ())
GLES2(glRenderbufferStorage, GLenum -> GLenum -> GLsizei -> GLsizei -> GL ())
GLES2(glSampleCoverage, GLclampf -> GLboolean -> GL ())
GLES2(glScissor, GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLES2(glShaderBinary, GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> GL ())
GLES2(glShaderSource, GLuint -> GLsizei -> Ptr CString -> Ptr GLint -> GL ())
GLES2(glStencilFunc, GLenum -> GLint -> GLuint -> GL ())
GLES2(glStencilFuncSeparate, GLenum -> GLenum -> GLint -> GLuint -> GL ())
GLES2(glStencilMask, GLuint -> GL ())
GLES2(glStencilMaskSeparate, GLenum -> GLuint -> GL ())
GLES2(glStencilOp, GLenum -> GLenum -> GLenum -> GL ())
GLES2(glStencilOpSeparate, GLenum -> GLenum -> GLenum -> GLenum -> GL ())
-- GLES2(glTexImage2D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> GL ())
GLES2(glTexImage2D, GLenum -> GLint -> GLenum -> GLuint -> GLuint -> GLint -> GLenum -> GLenum -> CString -> GL ())
GLES2(glTexParameterf, GLenum -> GLenum -> GLfloat -> GL ())
GLES2(glTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> GL ())
GLES2(glTexParameteri, GLenum -> GLenum -> GLint -> GL ())
GLES2(glTexParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLES2(glTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLES2(glUniform1f, GLint -> GLfloat -> GL ())
GLES2(glUniform1fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES2(glUniform1i, GLint -> GLint -> GL ())
GLES2(glUniform1iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLES2(glUniform2f, GLint -> GLfloat -> GLfloat -> GL ())
GLES2(glUniform2fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES2(glUniform2i, GLint -> GLint -> GLint -> GL ())
GLES2(glUniform2iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLES2(glUniform3f, GLint -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLES2(glUniform3fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES2(glUniform3i, GLint -> GLint -> GLint -> GLint -> GL ())
GLES2(glUniform3iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLES2(glUniform4f, GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLES2(glUniform4fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES2(glUniform4i, GLint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLES2(glUniform4iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLES2(glUniformMatrix2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES2(glUniformMatrix3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES2(glUniformMatrix4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES2(glUseProgram, GLuint -> GL ())
GLES2(glValidateProgram, GLuint -> GL ())
GLES2(glVertexAttrib1f, GLuint -> GLfloat -> GL ())
GLES2(glVertexAttrib1fv, GLuint -> Ptr GLfloat -> GL ())
GLES2(glVertexAttrib2f, GLuint -> GLfloat -> GLfloat -> GL ())
GLES2(glVertexAttrib2fv, GLuint -> Ptr GLfloat -> GL ())
GLES2(glVertexAttrib3f, GLuint -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLES2(glVertexAttrib3fv, GLuint -> Ptr GLfloat -> GL ())
GLES2(glVertexAttrib4f, GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLES2(glVertexAttrib4fv, GLuint -> Ptr GLfloat -> GL ())
GLES2(glVertexAttribPointer, GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> GL ())
GLES2(glViewport, GLint -> GLint -> GLsizei -> GLsizei -> GL ())

-- ** OpenGL ES 3.0

GLES3(glReadBuffer, GLenum -> GL ())
GLES3(glDrawRangeElements, GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GL ())
--GLES3(glTexImage3D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> GL ())
GLES3(glTexImage3D, GLenum -> GLint -> GLenum -> GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLenum -> CString -> GL ())
GLES3(glTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLES3(glCopyTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
--GLES3(glCompressedTexImage3D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> GL ())
GLES3(glCompressedTexImage3D, GLenum -> GLint -> GLenum -> GLuint -> GLuint -> GLuint -> GLint -> GLsizei -> CString -> GL ())
GLES3(glCompressedTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLES3(glGenQueries, GLsizei -> Ptr GLuint -> GL ())
GLES3(glDeleteQueries, GLsizei -> Ptr GLuint -> GL ())
GLES3(glIsQuery, GLuint -> GL GLboolean)
GLES3(glBeginQuery, GLenum -> GLuint -> GL ())
GLES3(glEndQuery, GLenum -> GL ())
GLES3(glGetQueryiv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLES3(glGetQueryObjectuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLES3(glUnmapBuffer, GLenum -> GL GLboolean)
GLES3(glGetBufferPointerv, GLenum -> GLenum -> Ptr (Ptr ()) -> GL ())
GLES3(glDrawBuffers, GLsizei -> Ptr GLenum -> GL ())
GLES3(glUniformMatrix2x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES3(glUniformMatrix3x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES3(glUniformMatrix2x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES3(glUniformMatrix4x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES3(glUniformMatrix3x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES3(glUniformMatrix4x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES3(glBlitFramebuffer, GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> GL ())
GLES3(glRenderbufferStorageMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GL ())
GLES3(glFramebufferTextureLayer, GLenum -> GLenum -> GLuint -> GLint -> GLint -> GL ())
GLES3(glMapBufferRange, GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> GL (Ptr ()))
GLES3(glFlushMappedBufferRange, GLenum -> GLintptr -> GLsizeiptr -> GL ())
GLES3(glBindVertexArray, GLuint -> GL ())
GLES3(glDeleteVertexArrays, GLsizei -> Ptr GLuint -> GL ())
GLES3(glGenVertexArrays, GLsizei -> Ptr GLuint -> GL ())
GLES3(glIsVertexArray, GLuint -> GL GLboolean)
GLES3(glGetIntegeri_v, GLenum -> GLuint -> Ptr GLint -> GL ())
GLES3(glBeginTransformFeedback, GLenum -> GL ())
GLES3(glEndTransformFeedback, GL ())
GLES3(glBindBufferRange, GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> GL ())
GLES3(glBindBufferBase, GLenum -> GLuint -> GLuint -> GL ())
GLES3(glTransformFeedbackVaryings, GLuint -> GLsizei -> Ptr CString -> GLenum -> GL ())
GLES3(glGetTransformFeedbackVarying, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> CString -> GL ())
GLES3(glVertexAttribIPointer, GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> GL ())
GLES3(glGetVertexAttribIiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLES3(glGetVertexAttribIuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLES3(glVertexAttribI4i, GLuint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLES3(glVertexAttribI4ui, GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLES3(glVertexAttribI4iv, GLuint -> Ptr GLint -> GL ())
GLES3(glVertexAttribI4uiv, GLuint -> Ptr GLuint -> GL ())
GLES3(glGetUniformuiv, GLuint -> GLint -> Ptr GLuint -> GL ())
GLES3(glGetFragDataLocation, GLuint -> CString-> GL GLint)
GLES3(glUniform1ui, GLint -> GLuint -> GL ())
GLES3(glUniform2ui, GLint -> GLuint -> GLuint -> GL ())
GLES3(glUniform3ui, GLint -> GLuint -> GLuint -> GLuint -> GL ())
GLES3(glUniform4ui, GLint -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLES3(glUniform1uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES3(glUniform2uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES3(glUniform3uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES3(glUniform4uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES3(glClearBufferiv, GLenum -> GLint -> Ptr GLint -> GL ())
GLES3(glClearBufferuiv, GLenum -> GLint -> Ptr GLuint -> GL ())
GLES3(glClearBufferfv, GLenum -> GLint -> Ptr GLfloat -> GL ())
GLES3(glClearBufferfi, GLenum -> GLint -> GLfloat -> GLint -> GL ())
GLES3(glGetStringi, GLenum -> GLuint -> GL CString)
GLES3(glCopyBufferSubData, GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> GL ())
GLES3(glGetUniformIndices, GLuint -> GLsizei -> Ptr CString -> Ptr GLuint -> GL ())
GLES3(glGetActiveUniformsiv, GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> GL ())
GLES3(glGetUniformBlockIndex, GLuint -> CString -> GL GLuint)
GLES3(glGetActiveUniformBlockiv, GLuint -> GLuint -> GLenum -> Ptr GLint -> GL ())
GLES3(glGetActiveUniformBlockName, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLES3(glUniformBlockBinding, GLuint -> GLuint -> GLuint -> GL ())
GLES3(glDrawArraysInstanced, GLenum -> GLint -> GLsizei -> GLsizei -> GL ())
GLES3(glDrawElementsInstanced, GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GL ())
GLES3(glFenceSync, GLenum -> GLbitfield -> GL GLsync)
GLES3(glIsSync, GLsync -> GL GLboolean)
GLES3(glDeleteSync, GLsync -> GL ())
GLES3(glClientWaitSync, GLsync -> GLbitfield -> GLuint64 -> GL GLenum)
GLES3(glWaitSync, GLsync -> GLbitfield -> GLuint64 -> GL ())
GLES3(glGetInteger64v, GLenum -> Ptr GLint64 -> GL ())
GLES3(glGetSynciv, GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> GL ())
GLES3(glGetInteger64i_v, GLenum -> GLuint -> Ptr GLint64 -> GL ())
GLES3(glGetBufferParameteri64v, GLenum -> GLenum -> Ptr GLint64 -> GL ())
GLES3(glGenSamplers, GLsizei -> Ptr GLuint -> GL ())
GLES3(glDeleteSamplers, GLsizei -> Ptr GLuint -> GL ())
GLES3(glIsSampler, GLuint -> GL GLboolean)
GLES3(glBindSampler, GLuint -> GLuint -> GL ())
GLES3(glSamplerParameteri, GLuint -> GLenum -> GLint -> GL ())
GLES3(glSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLES3(glSamplerParameterf, GLuint -> GLenum -> GLfloat -> GL ())
GLES3(glSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLES3(glGetSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLES3(glGetSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLES3(glVertexAttribDivisor, GLuint -> GLuint -> GL ())
GLES3(glBindTransformFeedback, GLenum -> GLuint -> GL ())
GLES3(glDeleteTransformFeedbacks, GLsizei -> Ptr GLuint -> GL ())
GLES3(glGenTransformFeedbacks, GLsizei -> Ptr GLuint -> GL ())
GLES3(glIsTransformFeedback, GLuint -> GL GLboolean)
GLES3(glPauseTransformFeedback, GL ())
GLES3(glResumeTransformFeedback, GL ())
GLES3(glGetProgramBinary, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> GL ())
GLES3(glProgramBinary, GLuint -> GLenum -> Ptr () -> GLsizei -> GL ())
GLES3(glProgramParameteri, GLuint -> GLenum -> GLint -> GL ())
GLES3(glInvalidateFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> GL ())
GLES3(glInvalidateSubFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLES3(glTexStorage2D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GL ())
GLES3(glTexStorage3D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GL ())
GLES3(glGetInternalformativ, GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> GL ())

-- ** Extensions

GL_EXT(glBindVertexArrayOES, GLuint -> GL ())
GL_EXT(glDeleteVertexArraysOES, GLsizei -> Ptr GLuint -> GL ())
GL_EXT(glGenVertexArraysOES, GLsizei -> Ptr GLuint -> GL ())
GL_EXT(glIsVertexArrayOES, GLuint -> GL GLboolean)

GL_EXT(glDrawTexiOES, GLint -> GLint -> GLint -> GLint -> GLint -> GL ())

GL_EXT(glMultiDrawArraysEXT, GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GL ())
GL_EXT(glMultiDrawElementsEXT, GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GL ())

GL_EXT(glDrawRangeElementsEXT, GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GL ())

-- ** OpenGL ES 3.1

GLES31(glDispatchCompute, GLuint -> GLuint -> GLuint -> GL ())
GLES31(glDispatchComputeIndirect, GLintptr -> GL ())
GLES31(glDrawArraysIndirect, GLenum -> Ptr () -> GL ())
GLES31(glDrawElementsIndirect, GLenum -> GLenum -> Ptr () -> GL ())
GLES31(glFramebufferParameteri, GLenum -> GLenum -> GLint -> GL ())
GLES31(glGetFramebufferParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLES31(glGetProgramInterfaceiv, GLuint -> GLenum -> GLenum -> Ptr GLint -> GL ())
GLES31(glGetProgramResourceIndex, GLuint -> GLenum -> Ptr GLchar -> GL ())
GLES31(glGetProgramResourceName, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> GL ())
GLES31(glGetProgramResourceiv, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> GL ())
GLES31(glGetProgramResourceLocation, GLuint -> GLenum -> Ptr GLchar -> GL ())
GLES31(glUseProgramStages, GLuint -> GLbitfield -> GLuint -> GL ())
GLES31(glActiveShaderProgram, GLuint -> GLuint -> GL ())
GLES31(glCreateShaderProgramv, GLenum -> GLsizei -> Ptr CString -> GL ())
GLES31(glBindProgramPipeline, GLuint -> GL ())
GLES31(glDeleteProgramPipelines, GLsizei -> Ptr GLuint -> GL ())
GLES31(glGenProgramPipelines, GLsizei -> Ptr GLuint -> GL ())
GLES31(glIsProgramPipeline, GLuint -> GL ())
GLES31(glGetProgramPipelineiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLES31(glProgramUniform1i, GLuint -> GLint -> GLint -> GL ())
GLES31(glProgramUniform2i, GLuint -> GLint -> GLint -> GLint -> GL ())
GLES31(glProgramUniform3i, GLuint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLES31(glProgramUniform4i, GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLES31(glProgramUniform1ui, GLuint -> GLint -> GLuint -> GL ())
GLES31(glProgramUniform2ui, GLuint -> GLint -> GLuint -> GLuint -> GL ())
GLES31(glProgramUniform3ui, GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GL ())
GLES31(glProgramUniform4ui, GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLES31(glProgramUniform1f, GLuint -> GLint -> GLfloat -> GL ())
GLES31(glProgramUniform2f, GLuint -> GLint -> GLfloat -> GLfloat -> GL ())
GLES31(glProgramUniform3f, GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLES31(glProgramUniform4f, GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLES31(glProgramUniform1iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLES31(glProgramUniform2iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLES31(glProgramUniform3iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLES31(glProgramUniform4iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLES31(glProgramUniform1uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES31(glProgramUniform2uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES31(glProgramUniform3uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES31(glProgramUniform4uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLES31(glProgramUniform1fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES31(glProgramUniform2fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES31(glProgramUniform3fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES31(glProgramUniform4fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix2x3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix3x2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix2x4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix4x2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix3x4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glProgramUniformMatrix4x3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLES31(glValidateProgramPipeline, GLuint -> GL ())
GLES31(glGetProgramPipelineInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> GL ())
GLES31(glBindImageTexture, GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> GL ())
GLES31(glGetBooleani_v, GLenum -> GLuint -> Ptr GLboolean -> GL ())
GLES31(glMemoryBarrier, GLbitfield -> GL ())
GLES31(glMemoryBarrierByRegion, GLbitfield -> GL ())
GLES31(glTexStorage2DMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> GL ())
GLES31(glGetMultisamplefv, GLenum -> GLuint -> Ptr GLfloat -> GL ())
GLES31(glSampleMaski, GLuint -> GLbitfield -> GL ())
GLES31(glGetTexLevelParameteriv, GLenum -> GLint -> GLenum -> Ptr GLint -> GL ())
GLES31(glGetTexLevelParameterfv, GLenum -> GLint -> GLenum -> Ptr GLfloat -> GL ())
GLES31(glBindVertexBuffer, GLuint -> GLuint -> GLintptr -> GLsizei -> GL ())
GLES31(glVertexAttribFormat, GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> GL ())
GLES31(glVertexAttribIFormat, GLuint -> GLint -> GLenum -> GLuint -> GL ())
GLES31(glVertexAttribBinding, GLuint -> GLuint -> GL ())
GLES31(glVertexBindingDivisor, GLuint -> GLuint -> GL ())

