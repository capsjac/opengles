{-# LANGUAGE CPP #-}
-- | OpenGL 4.5 Core Profile
-- <https://www.opengl.org/registry/api/GL/glcorearb.h>
module Graphics.OpenGL.CoreArb where
import Foreign
import Foreign.C.String
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Base.Proc (glGetProcAddress)

-- * Basic Types

-- | 64bit floating-point value
type GLdouble = Double

type GLuint64EXT = GLuint64

-- typedef void (APIENTRY  *GLDEBUGPROC)(GLenum source,GLenum type,GLuint id,GLenum severity,GLsizei length,const GLchar *message,const void *userParam);
-- #define GL_VERSION_4_3 1
type GLDebugProc = GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> CString -> Ptr () -> GL ()

foreign import ccall unsafe "wrapper"
    wrap_GLDebugProc :: GLDebugProc -> IO (FunPtr GLDebugProc)


-- * Bindings

#define GLAPI(_procname, _typ) \
foreign import ccall unsafe "dynamic" unwrap_/**/_procname :: FunPtr (_typ) -> _typ; \
_procname :: _typ; \
_procname = unwrap_/**/_procname (glGetProcAddress "_procname"); \
{-# NOINLINE _procname #-} \

-- foreign import ccall unsafe "dynamic"
--   unwrap_glActiveTexture :: FunPtr (GLenum -> GL ()) -> GLenum -> GL ();
-- glActiveTexture :: GLenum -> GL ();
-- glActiveTexture = unwrap_glActiveTexture (glGetProcAddress "glActiveTexture");

-- #ifndef __glcorearb_h_
-- #define __glcorearb_h_ 1
-- #if defined(_WIN32) && !defined(APIENTRY) && !defined(__CYGWIN__) && !defined(__SCITECH_SNAP__)
-- #ifndef WIN32_LEAN_AND_MEAN
-- #define WIN32_LEAN_AND_MEAN 1
-- #include <windows.h>
-- #ifndef APIENTRY
-- #define APIENTRY
-- #ifndef APIENTRYP
-- #define APIENTRYP APIENTRY *
-- #ifndef GLAPI
-- #define GLAPI extern
-- #include both <GL/glcorearb.h> and either of <GL/gl.h> or
-- #ifndef GL_VERSION_1_0
-- #define GL_VERSION_1_0 1
GLAPI(glCullFace, GLenum -> GL ())
GLAPI(glFrontFace, GLenum -> GL ())
GLAPI(glHint, GLenum -> GLenum -> GL ())
GLAPI(glLineWidth, GLfloat -> GL ())
GLAPI(glPointSize, GLfloat -> GL ())
GLAPI(glPolygonMode, GLenum -> GLenum -> GL ())
GLAPI(glScissor, GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glTexParameterf, GLenum -> GLenum -> GLfloat -> GL ())
GLAPI(glTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glTexParameteri, GLenum -> GLenum -> GLint -> GL ())
GLAPI(glTexParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glTexImage1D, GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glTexImage2D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glDrawBuffer, GLenum -> GL ())
GLAPI(glClear, GLbitfield -> GL ())
GLAPI(glClearColor, GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glClearStencil, GLint -> GL ())
GLAPI(glClearDepth, GLdouble -> GL ())
GLAPI(glStencilMask, GLuint -> GL ())
GLAPI(glColorMask, GLboolean -> GLboolean -> GLboolean -> GLboolean -> GL ())
GLAPI(glDepthMask, GLboolean -> GL ())
GLAPI(glDisable, GLenum -> GL ())
GLAPI(glEnable, GLenum -> GL ())
GLAPI(glFinish, GL ())
GLAPI(glFlush, GL ())
GLAPI(glBlendFunc, GLenum -> GLenum -> GL ())
GLAPI(glLogicOp, GLenum -> GL ())
GLAPI(glStencilFunc, GLenum -> GLint -> GLuint -> GL ())
GLAPI(glStencilOp, GLenum -> GLenum -> GLenum -> GL ())
GLAPI(glDepthFunc, GLenum -> GL ())
GLAPI(glPixelStoref, GLenum -> GLfloat -> GL ())
GLAPI(glPixelStorei, GLenum -> GLint -> GL ())
GLAPI(glReadBuffer, GLenum -> GL ())
GLAPI(glReadPixels, GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glGetBooleanv, GLenum -> Ptr GLboolean -> GL ())
GLAPI(glGetDoublev, GLenum -> Ptr GLdouble -> GL ())
GLAPI(glGetError, GL GLenum)
GLAPI(glGetFloatv, GLenum -> Ptr GLfloat -> GL ())
GLAPI(glGetIntegerv, GLenum -> Ptr GLint -> GL ())
GLAPI(glGetString, GLenum -> GL CString)
GLAPI(glGetTexImage, GLenum -> GLint -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glGetTexParameterfv, GLenum -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glGetTexParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetTexLevelParameterfv, GLenum -> GLint -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glGetTexLevelParameteriv, GLenum -> GLint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glIsEnabled, GLenum -> GL GLboolean)
GLAPI(glDepthRange, GLdouble -> GLdouble -> GL ())
GLAPI(glViewport, GLint -> GLint -> GLsizei -> GLsizei -> GL ())
-- #endif /* GL_VERSION_1_0 */
-- #ifndef GL_VERSION_1_1
-- #define GL_VERSION_1_1 1
-- #define GL_DEPTH_BUFFER_BIT               0x00000100
-- #define GL_STENCIL_BUFFER_BIT             0x00000400
-- #define GL_COLOR_BUFFER_BIT               0x00004000
-- #define GL_FALSE                          0
-- #define GL_TRUE                           1
-- #define GL_POINTS                         0x0000
-- #define GL_LINES                          0x0001
-- #define GL_LINE_LOOP                      0x0002
-- #define GL_LINE_STRIP                     0x0003
-- #define GL_TRIANGLES                      0x0004
-- #define GL_TRIANGLE_STRIP                 0x0005
-- #define GL_TRIANGLE_FAN                   0x0006
-- #define GL_QUADS                          0x0007
-- #define GL_NEVER                          0x0200
-- #define GL_LESS                           0x0201
-- #define GL_EQUAL                          0x0202
-- #define GL_LEQUAL                         0x0203
-- #define GL_GREATER                        0x0204
-- #define GL_NOTEQUAL                       0x0205
-- #define GL_GEQUAL                         0x0206
-- #define GL_ALWAYS                         0x0207
-- #define GL_ZERO                           0
-- #define GL_ONE                            1
-- #define GL_SRC_COLOR                      0x0300
-- #define GL_ONE_MINUS_SRC_COLOR            0x0301
-- #define GL_SRC_ALPHA                      0x0302
-- #define GL_ONE_MINUS_SRC_ALPHA            0x0303
-- #define GL_DST_ALPHA                      0x0304
-- #define GL_ONE_MINUS_DST_ALPHA            0x0305
-- #define GL_DST_COLOR                      0x0306
-- #define GL_ONE_MINUS_DST_COLOR            0x0307
-- #define GL_SRC_ALPHA_SATURATE             0x0308
-- #define GL_NONE                           0
-- #define GL_FRONT_LEFT                     0x0400
-- #define GL_FRONT_RIGHT                    0x0401
-- #define GL_BACK_LEFT                      0x0402
-- #define GL_BACK_RIGHT                     0x0403
-- #define GL_FRONT                          0x0404
-- #define GL_BACK                           0x0405
-- #define GL_LEFT                           0x0406
-- #define GL_RIGHT                          0x0407
-- #define GL_FRONT_AND_BACK                 0x0408
-- #define GL_NO_ERROR                       0
-- #define GL_INVALID_ENUM                   0x0500
-- #define GL_INVALID_VALUE                  0x0501
-- #define GL_INVALID_OPERATION              0x0502
-- #define GL_OUT_OF_MEMORY                  0x0505
-- #define GL_CW                             0x0900
-- #define GL_CCW                            0x0901
-- #define GL_POINT_SIZE                     0x0B11
-- #define GL_POINT_SIZE_RANGE               0x0B12
-- #define GL_POINT_SIZE_GRANULARITY         0x0B13
-- #define GL_LINE_SMOOTH                    0x0B20
-- #define GL_LINE_WIDTH                     0x0B21
-- #define GL_LINE_WIDTH_RANGE               0x0B22
-- #define GL_LINE_WIDTH_GRANULARITY         0x0B23
-- #define GL_POLYGON_MODE                   0x0B40
-- #define GL_POLYGON_SMOOTH                 0x0B41
-- #define GL_CULL_FACE                      0x0B44
-- #define GL_CULL_FACE_MODE                 0x0B45
-- #define GL_FRONT_FACE                     0x0B46
-- #define GL_DEPTH_RANGE                    0x0B70
-- #define GL_DEPTH_TEST                     0x0B71
-- #define GL_DEPTH_WRITEMASK                0x0B72
-- #define GL_DEPTH_CLEAR_VALUE              0x0B73
-- #define GL_DEPTH_FUNC                     0x0B74
-- #define GL_STENCIL_TEST                   0x0B90
-- #define GL_STENCIL_CLEAR_VALUE            0x0B91
-- #define GL_STENCIL_FUNC                   0x0B92
-- #define GL_STENCIL_VALUE_MASK             0x0B93
-- #define GL_STENCIL_FAIL                   0x0B94
-- #define GL_STENCIL_PASS_DEPTH_FAIL        0x0B95
-- #define GL_STENCIL_PASS_DEPTH_PASS        0x0B96
-- #define GL_STENCIL_REF                    0x0B97
-- #define GL_STENCIL_WRITEMASK              0x0B98
-- #define GL_VIEWPORT                       0x0BA2
-- #define GL_DITHER                         0x0BD0
-- #define GL_BLEND_DST                      0x0BE0
-- #define GL_BLEND_SRC                      0x0BE1
-- #define GL_BLEND                          0x0BE2
-- #define GL_LOGIC_OP_MODE                  0x0BF0
-- #define GL_COLOR_LOGIC_OP                 0x0BF2
-- #define GL_DRAW_BUFFER                    0x0C01
-- #define GL_READ_BUFFER                    0x0C02
-- #define GL_SCISSOR_BOX                    0x0C10
-- #define GL_SCISSOR_TEST                   0x0C11
-- #define GL_COLOR_CLEAR_VALUE              0x0C22
-- #define GL_COLOR_WRITEMASK                0x0C23
-- #define GL_DOUBLEBUFFER                   0x0C32
-- #define GL_STEREO                         0x0C33
-- #define GL_LINE_SMOOTH_HINT               0x0C52
-- #define GL_POLYGON_SMOOTH_HINT            0x0C53
-- #define GL_UNPACK_SWAP_BYTES              0x0CF0
-- #define GL_UNPACK_LSB_FIRST               0x0CF1
-- #define GL_UNPACK_ROW_LENGTH              0x0CF2
-- #define GL_UNPACK_SKIP_ROWS               0x0CF3
-- #define GL_UNPACK_SKIP_PIXELS             0x0CF4
-- #define GL_UNPACK_ALIGNMENT               0x0CF5
-- #define GL_PACK_SWAP_BYTES                0x0D00
-- #define GL_PACK_LSB_FIRST                 0x0D01
-- #define GL_PACK_ROW_LENGTH                0x0D02
-- #define GL_PACK_SKIP_ROWS                 0x0D03
-- #define GL_PACK_SKIP_PIXELS               0x0D04
-- #define GL_PACK_ALIGNMENT                 0x0D05
-- #define GL_MAX_TEXTURE_SIZE               0x0D33
-- #define GL_MAX_VIEWPORT_DIMS              0x0D3A
-- #define GL_SUBPIXEL_BITS                  0x0D50
-- #define GL_TEXTURE_1D                     0x0DE0
-- #define GL_TEXTURE_2D                     0x0DE1
-- #define GL_POLYGON_OFFSET_UNITS           0x2A00
-- #define GL_POLYGON_OFFSET_POINT           0x2A01
-- #define GL_POLYGON_OFFSET_LINE            0x2A02
-- #define GL_POLYGON_OFFSET_FILL            0x8037
-- #define GL_POLYGON_OFFSET_FACTOR          0x8038
-- #define GL_TEXTURE_BINDING_1D             0x8068
-- #define GL_TEXTURE_BINDING_2D             0x8069
-- #define GL_TEXTURE_WIDTH                  0x1000
-- #define GL_TEXTURE_HEIGHT                 0x1001
-- #define GL_TEXTURE_INTERNAL_FORMAT        0x1003
-- #define GL_TEXTURE_BORDER_COLOR           0x1004
-- #define GL_TEXTURE_RED_SIZE               0x805C
-- #define GL_TEXTURE_GREEN_SIZE             0x805D
-- #define GL_TEXTURE_BLUE_SIZE              0x805E
-- #define GL_TEXTURE_ALPHA_SIZE             0x805F
-- #define GL_DONT_CARE                      0x1100
-- #define GL_FASTEST                        0x1101
-- #define GL_NICEST                         0x1102
-- #define GL_BYTE                           0x1400
-- #define GL_UNSIGNED_BYTE                  0x1401
-- #define GL_SHORT                          0x1402
-- #define GL_UNSIGNED_SHORT                 0x1403
-- #define GL_INT                            0x1404
-- #define GL_UNSIGNED_INT                   0x1405
-- #define GL_FLOAT                          0x1406
-- #define GL_DOUBLE                         0x140A
-- #define GL_STACK_OVERFLOW                 0x0503
-- #define GL_STACK_UNDERFLOW                0x0504
-- #define GL_CLEAR                          0x1500
-- #define GL_AND                            0x1501
-- #define GL_AND_REVERSE                    0x1502
-- #define GL_COPY                           0x1503
-- #define GL_AND_INVERTED                   0x1504
-- #define GL_NOOP                           0x1505
-- #define GL_XOR                            0x1506
-- #define GL_OR                             0x1507
-- #define GL_NOR                            0x1508
-- #define GL_EQUIV                          0x1509
-- #define GL_INVERT                         0x150A
-- #define GL_OR_REVERSE                     0x150B
-- #define GL_COPY_INVERTED                  0x150C
-- #define GL_OR_INVERTED                    0x150D
-- #define GL_NAND                           0x150E
-- #define GL_SET                            0x150F
-- #define GL_TEXTURE                        0x1702
-- #define GL_COLOR                          0x1800
-- #define GL_DEPTH                          0x1801
-- #define GL_STENCIL                        0x1802
-- #define GL_STENCIL_INDEX                  0x1901
-- #define GL_DEPTH_COMPONENT                0x1902
-- #define GL_RED                            0x1903
-- #define GL_GREEN                          0x1904
-- #define GL_BLUE                           0x1905
-- #define GL_ALPHA                          0x1906
-- #define GL_RGB                            0x1907
-- #define GL_RGBA                           0x1908
-- #define GL_POINT                          0x1B00
-- #define GL_LINE                           0x1B01
-- #define GL_FILL                           0x1B02
-- #define GL_KEEP                           0x1E00
-- #define GL_REPLACE                        0x1E01
-- #define GL_INCR                           0x1E02
-- #define GL_DECR                           0x1E03
-- #define GL_VENDOR                         0x1F00
-- #define GL_RENDERER                       0x1F01
-- #define GL_VERSION                        0x1F02
-- #define GL_EXTENSIONS                     0x1F03
-- #define GL_NEAREST                        0x2600
-- #define GL_LINEAR                         0x2601
-- #define GL_NEAREST_MIPMAP_NEAREST         0x2700
-- #define GL_LINEAR_MIPMAP_NEAREST          0x2701
-- #define GL_NEAREST_MIPMAP_LINEAR          0x2702
-- #define GL_LINEAR_MIPMAP_LINEAR           0x2703
-- #define GL_TEXTURE_MAG_FILTER             0x2800
-- #define GL_TEXTURE_MIN_FILTER             0x2801
-- #define GL_TEXTURE_WRAP_S                 0x2802
-- #define GL_TEXTURE_WRAP_T                 0x2803
-- #define GL_PROXY_TEXTURE_1D               0x8063
-- #define GL_PROXY_TEXTURE_2D               0x8064
-- #define GL_REPEAT                         0x2901
-- #define GL_R3_G3_B2                       0x2A10
-- #define GL_RGB4                           0x804F
-- #define GL_RGB5                           0x8050
-- #define GL_RGB8                           0x8051
-- #define GL_RGB10                          0x8052
-- #define GL_RGB12                          0x8053
-- #define GL_RGB16                          0x8054
-- #define GL_RGBA2                          0x8055
-- #define GL_RGBA4                          0x8056
-- #define GL_RGB5_A1                        0x8057
-- #define GL_RGBA8                          0x8058
-- #define GL_RGB10_A2                       0x8059
-- #define GL_RGBA12                         0x805A
-- #define GL_RGBA16                         0x805B
-- #define GL_VERTEX_ARRAY                   0x8074
GLAPI(glDrawArrays, GLenum -> GLint -> GLsizei -> GL ())
GLAPI(glDrawElements, GLenum -> GLsizei -> GLenum -> Ptr () -> GL ())
GLAPI(glGetPointerv, GLenum -> Ptr (Ptr ()) -> GL ())
GLAPI(glPolygonOffset, GLfloat -> GLfloat -> GL ())
GLAPI(glCopyTexImage1D, GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GL ())
GLAPI(glCopyTexImage2D, GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GL ())
GLAPI(glCopyTexSubImage1D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GL ())
GLAPI(glCopyTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glTexSubImage1D, GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glBindTexture, GLenum -> GLuint -> GL ())
GLAPI(glDeleteTextures, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGenTextures, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glIsTexture, GLuint -> GL GLboolean)
-- #endif /* GL_VERSION_1_1 */
-- #ifndef GL_VERSION_1_2
-- #define GL_VERSION_1_2 1
-- #define GL_UNSIGNED_BYTE_3_3_2            0x8032
-- #define GL_UNSIGNED_SHORT_4_4_4_4         0x8033
-- #define GL_UNSIGNED_SHORT_5_5_5_1         0x8034
-- #define GL_UNSIGNED_INT_8_8_8_8           0x8035
-- #define GL_UNSIGNED_INT_10_10_10_2        0x8036
-- #define GL_TEXTURE_BINDING_3D             0x806A
-- #define GL_PACK_SKIP_IMAGES               0x806B
-- #define GL_PACK_IMAGE_HEIGHT              0x806C
-- #define GL_UNPACK_SKIP_IMAGES             0x806D
-- #define GL_UNPACK_IMAGE_HEIGHT            0x806E
-- #define GL_TEXTURE_3D                     0x806F
-- #define GL_PROXY_TEXTURE_3D               0x8070
-- #define GL_TEXTURE_DEPTH                  0x8071
-- #define GL_TEXTURE_WRAP_R                 0x8072
-- #define GL_MAX_3D_TEXTURE_SIZE            0x8073
-- #define GL_UNSIGNED_BYTE_2_3_3_REV        0x8362
-- #define GL_UNSIGNED_SHORT_5_6_5           0x8363
-- #define GL_UNSIGNED_SHORT_5_6_5_REV       0x8364
-- #define GL_UNSIGNED_SHORT_4_4_4_4_REV     0x8365
-- #define GL_UNSIGNED_SHORT_1_5_5_5_REV     0x8366
-- #define GL_UNSIGNED_INT_8_8_8_8_REV       0x8367
-- #define GL_UNSIGNED_INT_2_10_10_10_REV    0x8368
-- #define GL_BGR                            0x80E0
-- #define GL_BGRA                           0x80E1
-- #define GL_MAX_ELEMENTS_VERTICES          0x80E8
-- #define GL_MAX_ELEMENTS_INDICES           0x80E9
-- #define GL_CLAMP_TO_EDGE                  0x812F
-- #define GL_TEXTURE_MIN_LOD                0x813A
-- #define GL_TEXTURE_MAX_LOD                0x813B
-- #define GL_TEXTURE_BASE_LEVEL             0x813C
-- #define GL_TEXTURE_MAX_LEVEL              0x813D
-- #define GL_SMOOTH_POINT_SIZE_RANGE        0x0B12
-- #define GL_SMOOTH_POINT_SIZE_GRANULARITY  0x0B13
-- #define GL_SMOOTH_LINE_WIDTH_RANGE        0x0B22
-- #define GL_SMOOTH_LINE_WIDTH_GRANULARITY  0x0B23
-- #define GL_ALIASED_LINE_WIDTH_RANGE       0x846E
GLAPI(glDrawRangeElements, GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GL ())
GLAPI(glTexImage3D, GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glCopyTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
-- #endif /* GL_VERSION_1_2 */
-- #ifndef GL_VERSION_1_3
-- #define GL_VERSION_1_3 1
-- #define GL_TEXTURE0                       0x84C0
-- #define GL_TEXTURE1                       0x84C1
-- #define GL_TEXTURE2                       0x84C2
-- #define GL_TEXTURE3                       0x84C3
-- #define GL_TEXTURE4                       0x84C4
-- #define GL_TEXTURE5                       0x84C5
-- #define GL_TEXTURE6                       0x84C6
-- #define GL_TEXTURE7                       0x84C7
-- #define GL_TEXTURE8                       0x84C8
-- #define GL_TEXTURE9                       0x84C9
-- #define GL_TEXTURE10                      0x84CA
-- #define GL_TEXTURE11                      0x84CB
-- #define GL_TEXTURE12                      0x84CC
-- #define GL_TEXTURE13                      0x84CD
-- #define GL_TEXTURE14                      0x84CE
-- #define GL_TEXTURE15                      0x84CF
-- #define GL_TEXTURE16                      0x84D0
-- #define GL_TEXTURE17                      0x84D1
-- #define GL_TEXTURE18                      0x84D2
-- #define GL_TEXTURE19                      0x84D3
-- #define GL_TEXTURE20                      0x84D4
-- #define GL_TEXTURE21                      0x84D5
-- #define GL_TEXTURE22                      0x84D6
-- #define GL_TEXTURE23                      0x84D7
-- #define GL_TEXTURE24                      0x84D8
-- #define GL_TEXTURE25                      0x84D9
-- #define GL_TEXTURE26                      0x84DA
-- #define GL_TEXTURE27                      0x84DB
-- #define GL_TEXTURE28                      0x84DC
-- #define GL_TEXTURE29                      0x84DD
-- #define GL_TEXTURE30                      0x84DE
-- #define GL_TEXTURE31                      0x84DF
-- #define GL_ACTIVE_TEXTURE                 0x84E0
-- #define GL_MULTISAMPLE                    0x809D
-- #define GL_SAMPLE_ALPHA_TO_COVERAGE       0x809E
-- #define GL_SAMPLE_ALPHA_TO_ONE            0x809F
-- #define GL_SAMPLE_COVERAGE                0x80A0
-- #define GL_SAMPLE_BUFFERS                 0x80A8
-- #define GL_SAMPLES                        0x80A9
-- #define GL_SAMPLE_COVERAGE_VALUE          0x80AA
-- #define GL_SAMPLE_COVERAGE_INVERT         0x80AB
-- #define GL_TEXTURE_CUBE_MAP               0x8513
-- #define GL_TEXTURE_BINDING_CUBE_MAP       0x8514
-- #define GL_TEXTURE_CUBE_MAP_POSITIVE_X    0x8515
-- #define GL_TEXTURE_CUBE_MAP_NEGATIVE_X    0x8516
-- #define GL_TEXTURE_CUBE_MAP_POSITIVE_Y    0x8517
-- #define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y    0x8518
-- #define GL_TEXTURE_CUBE_MAP_POSITIVE_Z    0x8519
-- #define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z    0x851A
-- #define GL_PROXY_TEXTURE_CUBE_MAP         0x851B
-- #define GL_MAX_CUBE_MAP_TEXTURE_SIZE      0x851C
-- #define GL_COMPRESSED_RGB                 0x84ED
-- #define GL_COMPRESSED_RGBA                0x84EE
-- #define GL_TEXTURE_COMPRESSION_HINT       0x84EF
-- #define GL_TEXTURE_COMPRESSED_IMAGE_SIZE  0x86A0
-- #define GL_TEXTURE_COMPRESSED             0x86A1
-- #define GL_NUM_COMPRESSED_TEXTURE_FORMATS 0x86A2
-- #define GL_COMPRESSED_TEXTURE_FORMATS     0x86A3
-- #define GL_CLAMP_TO_BORDER                0x812D
GLAPI(glActiveTexture, GLenum -> GL ())
GLAPI(glSampleCoverage, GLfloat -> GLboolean -> GL ())
GLAPI(glCompressedTexImage3D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> GL ())
GLAPI(glCompressedTexImage2D, GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr () -> GL ())
GLAPI(glCompressedTexImage1D, GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr () -> GL ())
GLAPI(glCompressedTexSubImage3D, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glCompressedTexSubImage2D, GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glCompressedTexSubImage1D, GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetCompressedTexImage, GLenum -> GLint -> Ptr () -> GL ())
-- #endif /* GL_VERSION_1_3 */
-- #ifndef GL_VERSION_1_4
-- #define GL_VERSION_1_4 1
-- #define GL_BLEND_DST_RGB                  0x80C8
-- #define GL_BLEND_SRC_RGB                  0x80C9
-- #define GL_BLEND_DST_ALPHA                0x80CA
-- #define GL_BLEND_SRC_ALPHA                0x80CB
-- #define GL_POINT_FADE_THRESHOLD_SIZE      0x8128
-- #define GL_DEPTH_COMPONENT16              0x81A5
-- #define GL_DEPTH_COMPONENT24              0x81A6
-- #define GL_DEPTH_COMPONENT32              0x81A7
-- #define GL_MIRRORED_REPEAT                0x8370
-- #define GL_MAX_TEXTURE_LOD_BIAS           0x84FD
-- #define GL_TEXTURE_LOD_BIAS               0x8501
-- #define GL_INCR_WRAP                      0x8507
-- #define GL_DECR_WRAP                      0x8508
-- #define GL_TEXTURE_DEPTH_SIZE             0x884A
-- #define GL_TEXTURE_COMPARE_MODE           0x884C
-- #define GL_TEXTURE_COMPARE_FUNC           0x884D
-- #define GL_FUNC_ADD                       0x8006
-- #define GL_FUNC_SUBTRACT                  0x800A
-- #define GL_FUNC_REVERSE_SUBTRACT          0x800B
-- #define GL_MIN                            0x8007
-- #define GL_MAX                            0x8008
-- #define GL_CONSTANT_COLOR                 0x8001
-- #define GL_ONE_MINUS_CONSTANT_COLOR       0x8002
-- #define GL_CONSTANT_ALPHA                 0x8003
-- #define GL_ONE_MINUS_CONSTANT_ALPHA       0x8004
GLAPI(glBlendFuncSeparate, GLenum -> GLenum -> GLenum -> GLenum -> GL ())
GLAPI(glMultiDrawArrays, GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GL ())
GLAPI(glMultiDrawElements, GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> GL ())
GLAPI(glPointParameterf, GLenum -> GLfloat -> GL ())
GLAPI(glPointParameterfv, GLenum -> Ptr GLfloat -> GL ())
GLAPI(glPointParameteri, GLenum -> GLint -> GL ())
GLAPI(glPointParameteriv, GLenum -> Ptr GLint -> GL ())
GLAPI(glBlendColor, GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glBlendEquation, GLenum -> GL ())
-- #endif /* GL_VERSION_1_4 */
-- #ifndef GL_VERSION_1_5
-- #define GL_VERSION_1_5 1
-- #include <stddef.h>
-- #define GL_BUFFER_SIZE                    0x8764
-- #define GL_BUFFER_USAGE                   0x8765
-- #define GL_QUERY_COUNTER_BITS             0x8864
-- #define GL_CURRENT_QUERY                  0x8865
-- #define GL_QUERY_RESULT                   0x8866
-- #define GL_QUERY_RESULT_AVAILABLE         0x8867
-- #define GL_ARRAY_BUFFER                   0x8892
-- #define GL_ELEMENT_ARRAY_BUFFER           0x8893
-- #define GL_ARRAY_BUFFER_BINDING           0x8894
-- #define GL_ELEMENT_ARRAY_BUFFER_BINDING   0x8895
-- #define GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING 0x889F
-- #define GL_READ_ONLY                      0x88B8
-- #define GL_WRITE_ONLY                     0x88B9
-- #define GL_READ_WRITE                     0x88BA
-- #define GL_BUFFER_ACCESS                  0x88BB
-- #define GL_BUFFER_MAPPED                  0x88BC
-- #define GL_BUFFER_MAP_POINTER             0x88BD
-- #define GL_STREAM_DRAW                    0x88E0
-- #define GL_STREAM_READ                    0x88E1
-- #define GL_STREAM_COPY                    0x88E2
-- #define GL_STATIC_DRAW                    0x88E4
-- #define GL_STATIC_READ                    0x88E5
-- #define GL_STATIC_COPY                    0x88E6
-- #define GL_DYNAMIC_DRAW                   0x88E8
-- #define GL_DYNAMIC_READ                   0x88E9
-- #define GL_DYNAMIC_COPY                   0x88EA
-- #define GL_SAMPLES_PASSED                 0x8914
-- #define GL_SRC1_ALPHA                     0x8589
GLAPI(glGenQueries, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glDeleteQueries, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glIsQuery, GLuint -> GL GLboolean)
GLAPI(glBeginQuery, GLenum -> GLuint -> GL ())
GLAPI(glEndQuery, GLenum -> GL ())
GLAPI(glGetQueryiv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetQueryObjectiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetQueryObjectuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glBindBuffer, GLenum -> GLuint -> GL ())
GLAPI(glDeleteBuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGenBuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glIsBuffer, GLuint -> GL GLboolean)
GLAPI(glBufferData, GLenum -> GLsizeiptr -> Ptr () -> GLenum -> GL ())
GLAPI(glBufferSubData, GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> GL ())
GLAPI(glGetBufferSubData, GLenum -> GLintptr -> GLsizeiptr -> Ptr () -> GL ())
GLAPI(glMapBuffer, GLenum -> GLenum -> GL (Ptr ()))
GLAPI(glUnmapBuffer, GLenum -> GL GLboolean)
GLAPI(glGetBufferParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetBufferPointerv, GLenum -> GLenum -> Ptr (Ptr ()) -> GL ())
-- #endif /* GL_VERSION_1_5 */
-- #ifndef GL_VERSION_2_0
-- #define GL_VERSION_2_0 1
-- #define GL_BLEND_EQUATION_RGB             0x8009
-- #define GL_VERTEX_ATTRIB_ARRAY_ENABLED    0x8622
-- #define GL_VERTEX_ATTRIB_ARRAY_SIZE       0x8623
-- #define GL_VERTEX_ATTRIB_ARRAY_STRIDE     0x8624
-- #define GL_VERTEX_ATTRIB_ARRAY_TYPE       0x8625
-- #define GL_CURRENT_VERTEX_ATTRIB          0x8626
-- #define GL_VERTEX_PROGRAM_POINT_SIZE      0x8642
-- #define GL_VERTEX_ATTRIB_ARRAY_POINTER    0x8645
-- #define GL_STENCIL_BACK_FUNC              0x8800
-- #define GL_STENCIL_BACK_FAIL              0x8801
-- #define GL_STENCIL_BACK_PASS_DEPTH_FAIL   0x8802
-- #define GL_STENCIL_BACK_PASS_DEPTH_PASS   0x8803
-- #define GL_MAX_DRAW_BUFFERS               0x8824
-- #define GL_DRAW_BUFFER0                   0x8825
-- #define GL_DRAW_BUFFER1                   0x8826
-- #define GL_DRAW_BUFFER2                   0x8827
-- #define GL_DRAW_BUFFER3                   0x8828
-- #define GL_DRAW_BUFFER4                   0x8829
-- #define GL_DRAW_BUFFER5                   0x882A
-- #define GL_DRAW_BUFFER6                   0x882B
-- #define GL_DRAW_BUFFER7                   0x882C
-- #define GL_DRAW_BUFFER8                   0x882D
-- #define GL_DRAW_BUFFER9                   0x882E
-- #define GL_DRAW_BUFFER10                  0x882F
-- #define GL_DRAW_BUFFER11                  0x8830
-- #define GL_DRAW_BUFFER12                  0x8831
-- #define GL_DRAW_BUFFER13                  0x8832
-- #define GL_DRAW_BUFFER14                  0x8833
-- #define GL_DRAW_BUFFER15                  0x8834
-- #define GL_BLEND_EQUATION_ALPHA           0x883D
-- #define GL_MAX_VERTEX_ATTRIBS             0x8869
-- #define GL_VERTEX_ATTRIB_ARRAY_NORMALIZED 0x886A
-- #define GL_MAX_TEXTURE_IMAGE_UNITS        0x8872
-- #define GL_FRAGMENT_SHADER                0x8B30
-- #define GL_VERTEX_SHADER                  0x8B31
-- #define GL_MAX_FRAGMENT_UNIFORM_COMPONENTS 0x8B49
-- #define GL_MAX_VERTEX_UNIFORM_COMPONENTS  0x8B4A
-- #define GL_MAX_VARYING_FLOATS             0x8B4B
-- #define GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS 0x8B4C
-- #define GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS 0x8B4D
-- #define GL_SHADER_TYPE                    0x8B4F
-- #define GL_FLOAT_VEC2                     0x8B50
-- #define GL_FLOAT_VEC3                     0x8B51
-- #define GL_FLOAT_VEC4                     0x8B52
-- #define GL_INT_VEC2                       0x8B53
-- #define GL_INT_VEC3                       0x8B54
-- #define GL_INT_VEC4                       0x8B55
-- #define GL_BOOL                           0x8B56
-- #define GL_BOOL_VEC2                      0x8B57
-- #define GL_BOOL_VEC3                      0x8B58
-- #define GL_BOOL_VEC4                      0x8B59
-- #define GL_FLOAT_MAT2                     0x8B5A
-- #define GL_FLOAT_MAT3                     0x8B5B
-- #define GL_FLOAT_MAT4                     0x8B5C
-- #define GL_SAMPLER_1D                     0x8B5D
-- #define GL_SAMPLER_2D                     0x8B5E
-- #define GL_SAMPLER_3D                     0x8B5F
-- #define GL_SAMPLER_CUBE                   0x8B60
-- #define GL_SAMPLER_1D_SHADOW              0x8B61
-- #define GL_SAMPLER_2D_SHADOW              0x8B62
-- #define GL_DELETE_STATUS                  0x8B80
-- #define GL_COMPILE_STATUS                 0x8B81
-- #define GL_LINK_STATUS                    0x8B82
-- #define GL_VALIDATE_STATUS                0x8B83
-- #define GL_INFO_LOG_LENGTH                0x8B84
-- #define GL_ATTACHED_SHADERS               0x8B85
-- #define GL_ACTIVE_UNIFORMS                0x8B86
-- #define GL_ACTIVE_UNIFORM_MAX_LENGTH      0x8B87
-- #define GL_SHADER_SOURCE_LENGTH           0x8B88
-- #define GL_ACTIVE_ATTRIBUTES              0x8B89
-- #define GL_ACTIVE_ATTRIBUTE_MAX_LENGTH    0x8B8A
-- #define GL_FRAGMENT_SHADER_DERIVATIVE_HINT 0x8B8B
-- #define GL_SHADING_LANGUAGE_VERSION       0x8B8C
-- #define GL_CURRENT_PROGRAM                0x8B8D
-- #define GL_POINT_SPRITE_COORD_ORIGIN      0x8CA0
-- #define GL_LOWER_LEFT                     0x8CA1
-- #define GL_UPPER_LEFT                     0x8CA2
-- #define GL_STENCIL_BACK_REF               0x8CA3
-- #define GL_STENCIL_BACK_VALUE_MASK        0x8CA4
-- #define GL_STENCIL_BACK_WRITEMASK         0x8CA5
GLAPI(glBlendEquationSeparate, GLenum -> GLenum -> GL ())
GLAPI(glDrawBuffers, GLsizei -> Ptr GLenum -> GL ())
GLAPI(glStencilOpSeparate, GLenum -> GLenum -> GLenum -> GLenum -> GL ())
GLAPI(glStencilFuncSeparate, GLenum -> GLenum -> GLint -> GLuint -> GL ())
GLAPI(glStencilMaskSeparate, GLenum -> GLuint -> GL ())
GLAPI(glAttachShader, GLuint -> GLuint -> GL ())
GLAPI(glBindAttribLocation, GLuint -> GLuint -> CString -> GL ())
GLAPI(glCompileShader, GLuint -> GL ())
GLAPI(glCreateProgram, GL GLuint)
GLAPI(glCreateShader, GLenum -> GL GLuint)
GLAPI(glDeleteProgram, GLuint -> GL ())
GLAPI(glDeleteShader, GLuint -> GL ())
GLAPI(glDetachShader, GLuint -> GLuint -> GL ())
GLAPI(glDisableVertexAttribArray, GLuint -> GL ())
GLAPI(glEnableVertexAttribArray, GLuint -> GL ())
GLAPI(glGetActiveAttrib, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> CString -> GL ())
GLAPI(glGetActiveUniform, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> CString -> GL ())
GLAPI(glGetAttachedShaders, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGetAttribLocation, GLuint -> CString -> GL GLint)
GLAPI(glGetProgramiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetProgramInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glGetShaderiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetShaderInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glGetShaderSource, GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glGetUniformLocation, GLuint -> CString -> GL GLint)
GLAPI(glGetUniformfv, GLuint -> GLint -> Ptr GLfloat -> GL ())
GLAPI(glGetUniformiv, GLuint -> GLint -> Ptr GLint -> GL ())
GLAPI(glGetVertexAttribdv, GLuint -> GLenum -> Ptr GLdouble -> GL ())
GLAPI(glGetVertexAttribfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glGetVertexAttribiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetVertexAttribPointerv, GLuint -> GLenum -> Ptr (Ptr ()) -> GL ())
GLAPI(glIsProgram, GLuint -> GL GLboolean)
GLAPI(glIsShader, GLuint -> GL GLboolean)
GLAPI(glLinkProgram, GLuint -> GL ())
GLAPI(glShaderSource, GLuint -> GLsizei -> Ptr CString -> Ptr GLint -> GL ())
GLAPI(glUseProgram, GLuint -> GL ())
GLAPI(glUniform1f, GLint -> GLfloat -> GL ())
GLAPI(glUniform2f, GLint -> GLfloat -> GLfloat -> GL ())
GLAPI(glUniform3f, GLint -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glUniform4f, GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glUniform1i, GLint -> GLint -> GL ())
GLAPI(glUniform2i, GLint -> GLint -> GLint -> GL ())
GLAPI(glUniform3i, GLint -> GLint -> GLint -> GLint -> GL ())
GLAPI(glUniform4i, GLint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLAPI(glUniform1fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glUniform2fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glUniform3fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glUniform4fv, GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glUniform1iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glUniform2iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glUniform3iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glUniform4iv, GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glUniformMatrix2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glUniformMatrix3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glUniformMatrix4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glValidateProgram, GLuint -> GL ())
GLAPI(glVertexAttrib1d, GLuint -> GLdouble -> GL ())
GLAPI(glVertexAttrib1dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttrib1f, GLuint -> GLfloat -> GL ())
GLAPI(glVertexAttrib1fv, GLuint -> Ptr GLfloat -> GL ())
GLAPI(glVertexAttrib1s, GLuint -> GLshort -> GL ())
GLAPI(glVertexAttrib1sv, GLuint -> Ptr GLshort -> GL ())
GLAPI(glVertexAttrib2d, GLuint -> GLdouble -> GLdouble -> GL ())
GLAPI(glVertexAttrib2dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttrib2f, GLuint -> GLfloat -> GLfloat -> GL ())
GLAPI(glVertexAttrib2fv, GLuint -> Ptr GLfloat -> GL ())
GLAPI(glVertexAttrib2s, GLuint -> GLshort -> GLshort -> GL ())
GLAPI(glVertexAttrib2sv, GLuint -> Ptr GLshort -> GL ())
GLAPI(glVertexAttrib3d, GLuint -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glVertexAttrib3dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttrib3f, GLuint -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glVertexAttrib3fv, GLuint -> Ptr GLfloat -> GL ())
GLAPI(glVertexAttrib3s, GLuint -> GLshort -> GLshort -> GLshort -> GL ())
GLAPI(glVertexAttrib3sv, GLuint -> Ptr GLshort -> GL ())
GLAPI(glVertexAttrib4Nbv, GLuint -> Ptr GLbyte -> GL ())
GLAPI(glVertexAttrib4Niv, GLuint -> Ptr GLint -> GL ())
GLAPI(glVertexAttrib4Nsv, GLuint -> Ptr GLshort -> GL ())
GLAPI(glVertexAttrib4Nub, GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GL ())
GLAPI(glVertexAttrib4Nubv, GLuint -> Ptr GLubyte -> GL ())
GLAPI(glVertexAttrib4Nuiv, GLuint -> Ptr GLuint -> GL ())
GLAPI(glVertexAttrib4Nusv, GLuint -> Ptr GLushort -> GL ())
GLAPI(glVertexAttrib4bv, GLuint -> Ptr GLbyte -> GL ())
GLAPI(glVertexAttrib4d, GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glVertexAttrib4dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttrib4f, GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glVertexAttrib4fv, GLuint -> Ptr GLfloat -> GL ())
GLAPI(glVertexAttrib4iv, GLuint -> Ptr GLint -> GL ())
GLAPI(glVertexAttrib4s, GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> GL ())
GLAPI(glVertexAttrib4sv, GLuint -> Ptr GLshort -> GL ())
GLAPI(glVertexAttrib4ubv, GLuint -> Ptr GLubyte -> GL ())
GLAPI(glVertexAttrib4uiv, GLuint -> Ptr GLuint -> GL ())
GLAPI(glVertexAttrib4usv, GLuint -> Ptr GLushort -> GL ())
GLAPI(glVertexAttribPointer, GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr () -> GL ())
-- #endif /* GL_VERSION_2_0 */
-- #ifndef GL_VERSION_2_1
-- #define GL_VERSION_2_1 1
-- #define GL_PIXEL_PACK_BUFFER              0x88EB
-- #define GL_PIXEL_UNPACK_BUFFER            0x88EC
-- #define GL_PIXEL_PACK_BUFFER_BINDING      0x88ED
-- #define GL_PIXEL_UNPACK_BUFFER_BINDING    0x88EF
-- #define GL_FLOAT_MAT2x3                   0x8B65
-- #define GL_FLOAT_MAT2x4                   0x8B66
-- #define GL_FLOAT_MAT3x2                   0x8B67
-- #define GL_FLOAT_MAT3x4                   0x8B68
-- #define GL_FLOAT_MAT4x2                   0x8B69
-- #define GL_FLOAT_MAT4x3                   0x8B6A
-- #define GL_SRGB                           0x8C40
-- #define GL_SRGB8                          0x8C41
-- #define GL_SRGB_ALPHA                     0x8C42
-- #define GL_SRGB8_ALPHA8                   0x8C43
-- #define GL_COMPRESSED_SRGB                0x8C48
-- #define GL_COMPRESSED_SRGB_ALPHA          0x8C49
GLAPI(glUniformMatrix2x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glUniformMatrix3x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glUniformMatrix2x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glUniformMatrix4x2fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glUniformMatrix3x4fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glUniformMatrix4x3fv, GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
-- #endif /* GL_VERSION_2_1 */
-- #ifndef GL_VERSION_3_0
-- #define GL_VERSION_3_0 1
-- #define GL_COMPARE_REF_TO_TEXTURE         0x884E
-- #define GL_CLIP_DISTANCE0                 0x3000
-- #define GL_CLIP_DISTANCE1                 0x3001
-- #define GL_CLIP_DISTANCE2                 0x3002
-- #define GL_CLIP_DISTANCE3                 0x3003
-- #define GL_CLIP_DISTANCE4                 0x3004
-- #define GL_CLIP_DISTANCE5                 0x3005
-- #define GL_CLIP_DISTANCE6                 0x3006
-- #define GL_CLIP_DISTANCE7                 0x3007
-- #define GL_MAX_CLIP_DISTANCES             0x0D32
-- #define GL_MAJOR_VERSION                  0x821B
-- #define GL_MINOR_VERSION                  0x821C
-- #define GL_NUM_EXTENSIONS                 0x821D
-- #define GL_CONTEXT_FLAGS                  0x821E
-- #define GL_COMPRESSED_RED                 0x8225
-- #define GL_COMPRESSED_RG                  0x8226
-- #define GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT 0x00000001
-- #define GL_RGBA32F                        0x8814
-- #define GL_RGB32F                         0x8815
-- #define GL_RGBA16F                        0x881A
-- #define GL_RGB16F                         0x881B
-- #define GL_VERTEX_ATTRIB_ARRAY_INTEGER    0x88FD
-- #define GL_MAX_ARRAY_TEXTURE_LAYERS       0x88FF
-- #define GL_MIN_PROGRAM_TEXEL_OFFSET       0x8904
-- #define GL_MAX_PROGRAM_TEXEL_OFFSET       0x8905
-- #define GL_CLAMP_READ_COLOR               0x891C
-- #define GL_FIXED_ONLY                     0x891D
-- #define GL_MAX_VARYING_COMPONENTS         0x8B4B
-- #define GL_TEXTURE_1D_ARRAY               0x8C18
-- #define GL_PROXY_TEXTURE_1D_ARRAY         0x8C19
-- #define GL_TEXTURE_2D_ARRAY               0x8C1A
-- #define GL_PROXY_TEXTURE_2D_ARRAY         0x8C1B
-- #define GL_TEXTURE_BINDING_1D_ARRAY       0x8C1C
-- #define GL_TEXTURE_BINDING_2D_ARRAY       0x8C1D
-- #define GL_R11F_G11F_B10F                 0x8C3A
-- #define GL_UNSIGNED_INT_10F_11F_11F_REV   0x8C3B
-- #define GL_RGB9_E5                        0x8C3D
-- #define GL_UNSIGNED_INT_5_9_9_9_REV       0x8C3E
-- #define GL_TEXTURE_SHARED_SIZE            0x8C3F
-- #define GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH 0x8C76
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_MODE 0x8C7F
-- #define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS 0x8C80
-- #define GL_TRANSFORM_FEEDBACK_VARYINGS    0x8C83
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_START 0x8C84
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_SIZE 0x8C85
-- #define GL_PRIMITIVES_GENERATED           0x8C87
-- #define GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN 0x8C88
-- #define GL_RASTERIZER_DISCARD             0x8C89
-- #define GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS 0x8C8A
-- #define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS 0x8C8B
-- #define GL_INTERLEAVED_ATTRIBS            0x8C8C
-- #define GL_SEPARATE_ATTRIBS               0x8C8D
-- #define GL_TRANSFORM_FEEDBACK_BUFFER      0x8C8E
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_BINDING 0x8C8F
-- #define GL_RGBA32UI                       0x8D70
-- #define GL_RGB32UI                        0x8D71
-- #define GL_RGBA16UI                       0x8D76
-- #define GL_RGB16UI                        0x8D77
-- #define GL_RGBA8UI                        0x8D7C
-- #define GL_RGB8UI                         0x8D7D
-- #define GL_RGBA32I                        0x8D82
-- #define GL_RGB32I                         0x8D83
-- #define GL_RGBA16I                        0x8D88
-- #define GL_RGB16I                         0x8D89
-- #define GL_RGBA8I                         0x8D8E
-- #define GL_RGB8I                          0x8D8F
-- #define GL_RED_INTEGER                    0x8D94
-- #define GL_GREEN_INTEGER                  0x8D95
-- #define GL_BLUE_INTEGER                   0x8D96
-- #define GL_RGB_INTEGER                    0x8D98
-- #define GL_RGBA_INTEGER                   0x8D99
-- #define GL_BGR_INTEGER                    0x8D9A
-- #define GL_BGRA_INTEGER                   0x8D9B
-- #define GL_SAMPLER_1D_ARRAY               0x8DC0
-- #define GL_SAMPLER_2D_ARRAY               0x8DC1
-- #define GL_SAMPLER_1D_ARRAY_SHADOW        0x8DC3
-- #define GL_SAMPLER_2D_ARRAY_SHADOW        0x8DC4
-- #define GL_SAMPLER_CUBE_SHADOW            0x8DC5
-- #define GL_UNSIGNED_INT_VEC2              0x8DC6
-- #define GL_UNSIGNED_INT_VEC3              0x8DC7
-- #define GL_UNSIGNED_INT_VEC4              0x8DC8
-- #define GL_INT_SAMPLER_1D                 0x8DC9
-- #define GL_INT_SAMPLER_2D                 0x8DCA
-- #define GL_INT_SAMPLER_3D                 0x8DCB
-- #define GL_INT_SAMPLER_CUBE               0x8DCC
-- #define GL_INT_SAMPLER_1D_ARRAY           0x8DCE
-- #define GL_INT_SAMPLER_2D_ARRAY           0x8DCF
-- #define GL_UNSIGNED_INT_SAMPLER_1D        0x8DD1
-- #define GL_UNSIGNED_INT_SAMPLER_2D        0x8DD2
-- #define GL_UNSIGNED_INT_SAMPLER_3D        0x8DD3
-- #define GL_UNSIGNED_INT_SAMPLER_CUBE      0x8DD4
-- #define GL_UNSIGNED_INT_SAMPLER_1D_ARRAY  0x8DD6
-- #define GL_UNSIGNED_INT_SAMPLER_2D_ARRAY  0x8DD7
-- #define GL_QUERY_WAIT                     0x8E13
-- #define GL_QUERY_NO_WAIT                  0x8E14
-- #define GL_QUERY_BY_REGION_WAIT           0x8E15
-- #define GL_QUERY_BY_REGION_NO_WAIT        0x8E16
-- #define GL_BUFFER_ACCESS_FLAGS            0x911F
-- #define GL_BUFFER_MAP_LENGTH              0x9120
-- #define GL_BUFFER_MAP_OFFSET              0x9121
-- #define GL_DEPTH_COMPONENT32F             0x8CAC
-- #define GL_DEPTH32F_STENCIL8              0x8CAD
-- #define GL_FLOAT_32_UNSIGNED_INT_24_8_REV 0x8DAD
-- #define GL_INVALID_FRAMEBUFFER_OPERATION  0x0506
-- #define GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING 0x8210
-- #define GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE 0x8211
-- #define GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE 0x8212
-- #define GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE 0x8213
-- #define GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE 0x8214
-- #define GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE 0x8215
-- #define GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE 0x8216
-- #define GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE 0x8217
-- #define GL_FRAMEBUFFER_DEFAULT            0x8218
-- #define GL_FRAMEBUFFER_UNDEFINED          0x8219
-- #define GL_DEPTH_STENCIL_ATTACHMENT       0x821A
-- #define GL_MAX_RENDERBUFFER_SIZE          0x84E8
-- #define GL_DEPTH_STENCIL                  0x84F9
-- #define GL_UNSIGNED_INT_24_8              0x84FA
-- #define GL_DEPTH24_STENCIL8               0x88F0
-- #define GL_TEXTURE_STENCIL_SIZE           0x88F1
-- #define GL_TEXTURE_RED_TYPE               0x8C10
-- #define GL_TEXTURE_GREEN_TYPE             0x8C11
-- #define GL_TEXTURE_BLUE_TYPE              0x8C12
-- #define GL_TEXTURE_ALPHA_TYPE             0x8C13
-- #define GL_TEXTURE_DEPTH_TYPE             0x8C16
-- #define GL_UNSIGNED_NORMALIZED            0x8C17
-- #define GL_FRAMEBUFFER_BINDING            0x8CA6
-- #define GL_DRAW_FRAMEBUFFER_BINDING       0x8CA6
-- #define GL_RENDERBUFFER_BINDING           0x8CA7
-- #define GL_READ_FRAMEBUFFER               0x8CA8
-- #define GL_DRAW_FRAMEBUFFER               0x8CA9
-- #define GL_READ_FRAMEBUFFER_BINDING       0x8CAA
-- #define GL_RENDERBUFFER_SAMPLES           0x8CAB
-- #define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE 0x8CD0
-- #define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME 0x8CD1
-- #define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL 0x8CD2
-- #define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE 0x8CD3
-- #define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER 0x8CD4
-- #define GL_FRAMEBUFFER_COMPLETE           0x8CD5
-- #define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT 0x8CD6
-- #define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT 0x8CD7
-- #define GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER 0x8CDB
-- #define GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER 0x8CDC
-- #define GL_FRAMEBUFFER_UNSUPPORTED        0x8CDD
-- #define GL_MAX_COLOR_ATTACHMENTS          0x8CDF
-- #define GL_COLOR_ATTACHMENT0              0x8CE0
-- #define GL_COLOR_ATTACHMENT1              0x8CE1
-- #define GL_COLOR_ATTACHMENT2              0x8CE2
-- #define GL_COLOR_ATTACHMENT3              0x8CE3
-- #define GL_COLOR_ATTACHMENT4              0x8CE4
-- #define GL_COLOR_ATTACHMENT5              0x8CE5
-- #define GL_COLOR_ATTACHMENT6              0x8CE6
-- #define GL_COLOR_ATTACHMENT7              0x8CE7
-- #define GL_COLOR_ATTACHMENT8              0x8CE8
-- #define GL_COLOR_ATTACHMENT9              0x8CE9
-- #define GL_COLOR_ATTACHMENT10             0x8CEA
-- #define GL_COLOR_ATTACHMENT11             0x8CEB
-- #define GL_COLOR_ATTACHMENT12             0x8CEC
-- #define GL_COLOR_ATTACHMENT13             0x8CED
-- #define GL_COLOR_ATTACHMENT14             0x8CEE
-- #define GL_COLOR_ATTACHMENT15             0x8CEF
-- #define GL_DEPTH_ATTACHMENT               0x8D00
-- #define GL_STENCIL_ATTACHMENT             0x8D20
-- #define GL_FRAMEBUFFER                    0x8D40
-- #define GL_RENDERBUFFER                   0x8D41
-- #define GL_RENDERBUFFER_WIDTH             0x8D42
-- #define GL_RENDERBUFFER_HEIGHT            0x8D43
-- #define GL_RENDERBUFFER_INTERNAL_FORMAT   0x8D44
-- #define GL_STENCIL_INDEX1                 0x8D46
-- #define GL_STENCIL_INDEX4                 0x8D47
-- #define GL_STENCIL_INDEX8                 0x8D48
-- #define GL_STENCIL_INDEX16                0x8D49
-- #define GL_RENDERBUFFER_RED_SIZE          0x8D50
-- #define GL_RENDERBUFFER_GREEN_SIZE        0x8D51
-- #define GL_RENDERBUFFER_BLUE_SIZE         0x8D52
-- #define GL_RENDERBUFFER_ALPHA_SIZE        0x8D53
-- #define GL_RENDERBUFFER_DEPTH_SIZE        0x8D54
-- #define GL_RENDERBUFFER_STENCIL_SIZE      0x8D55
-- #define GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE 0x8D56
-- #define GL_MAX_SAMPLES                    0x8D57
-- #define GL_FRAMEBUFFER_SRGB               0x8DB9
-- #define GL_HALF_FLOAT                     0x140B
-- #define GL_MAP_READ_BIT                   0x0001
-- #define GL_MAP_WRITE_BIT                  0x0002
-- #define GL_MAP_INVALIDATE_RANGE_BIT       0x0004
-- #define GL_MAP_INVALIDATE_BUFFER_BIT      0x0008
-- #define GL_MAP_FLUSH_EXPLICIT_BIT         0x0010
-- #define GL_MAP_UNSYNCHRONIZED_BIT         0x0020
-- #define GL_COMPRESSED_RED_RGTC1           0x8DBB
-- #define GL_COMPRESSED_SIGNED_RED_RGTC1    0x8DBC
-- #define GL_COMPRESSED_RG_RGTC2            0x8DBD
-- #define GL_COMPRESSED_SIGNED_RG_RGTC2     0x8DBE
-- #define GL_RG                             0x8227
-- #define GL_RG_INTEGER                     0x8228
-- #define GL_R8                             0x8229
-- #define GL_R16                            0x822A
-- #define GL_RG8                            0x822B
-- #define GL_RG16                           0x822C
-- #define GL_R16F                           0x822D
-- #define GL_R32F                           0x822E
-- #define GL_RG16F                          0x822F
-- #define GL_RG32F                          0x8230
-- #define GL_R8I                            0x8231
-- #define GL_R8UI                           0x8232
-- #define GL_R16I                           0x8233
-- #define GL_R16UI                          0x8234
-- #define GL_R32I                           0x8235
-- #define GL_R32UI                          0x8236
-- #define GL_RG8I                           0x8237
-- #define GL_RG8UI                          0x8238
-- #define GL_RG16I                          0x8239
-- #define GL_RG16UI                         0x823A
-- #define GL_RG32I                          0x823B
-- #define GL_RG32UI                         0x823C
-- #define GL_VERTEX_ARRAY_BINDING           0x85B5
GLAPI(glColorMaski, GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> GL ())
GLAPI(glGetBooleani_v, GLenum -> GLuint -> Ptr GLboolean -> GL ())
GLAPI(glGetIntegeri_v, GLenum -> GLuint -> Ptr GLint -> GL ())
GLAPI(glEnablei, GLenum -> GLuint -> GL ())
GLAPI(glDisablei, GLenum -> GLuint -> GL ())
GLAPI(glIsEnabledi, GLenum -> GLuint -> GL GLboolean)
GLAPI(glBeginTransformFeedback, GLenum -> GL ())
GLAPI(glEndTransformFeedback, GL ())
GLAPI(glBindBufferRange, GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> GL ())
GLAPI(glBindBufferBase, GLenum -> GLuint -> GLuint -> GL ())
GLAPI(glTransformFeedbackVaryings, GLuint -> GLsizei -> Ptr CString -> GLenum -> GL ())
GLAPI(glGetTransformFeedbackVarying, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> CString -> GL ())
GLAPI(glClampColor, GLenum -> GLenum -> GL ())
GLAPI(glBeginConditionalRender, GLuint -> GLenum -> GL ())
GLAPI(glEndConditionalRender, GL ())
GLAPI(glVertexAttribIPointer, GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetVertexAttribIiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetVertexAttribIuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribI1i, GLuint -> GLint -> GL ())
GLAPI(glVertexAttribI2i, GLuint -> GLint -> GLint -> GL ())
GLAPI(glVertexAttribI3i, GLuint -> GLint -> GLint -> GLint -> GL ())
GLAPI(glVertexAttribI4i, GLuint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLAPI(glVertexAttribI1ui, GLuint -> GLuint -> GL ())
GLAPI(glVertexAttribI2ui, GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glVertexAttribI3ui, GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glVertexAttribI4ui, GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glVertexAttribI1iv, GLuint -> Ptr GLint -> GL ())
GLAPI(glVertexAttribI2iv, GLuint -> Ptr GLint -> GL ())
GLAPI(glVertexAttribI3iv, GLuint -> Ptr GLint -> GL ())
GLAPI(glVertexAttribI4iv, GLuint -> Ptr GLint -> GL ())
GLAPI(glVertexAttribI1uiv, GLuint -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribI2uiv, GLuint -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribI3uiv, GLuint -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribI4uiv, GLuint -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribI4bv, GLuint -> Ptr GLbyte -> GL ())
GLAPI(glVertexAttribI4sv, GLuint -> Ptr GLshort -> GL ())
GLAPI(glVertexAttribI4ubv, GLuint -> Ptr GLubyte -> GL ())
GLAPI(glVertexAttribI4usv, GLuint -> Ptr GLushort -> GL ())
GLAPI(glGetUniformuiv, GLuint -> GLint -> Ptr GLuint -> GL ())
GLAPI(glBindFragDataLocation, GLuint -> GLuint -> CString -> GL ())
GLAPI(glGetFragDataLocation, GLuint -> CString -> GL GLint)
GLAPI(glUniform1ui, GLint -> GLuint -> GL ())
GLAPI(glUniform2ui, GLint -> GLuint -> GLuint -> GL ())
GLAPI(glUniform3ui, GLint -> GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glUniform4ui, GLint -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glUniform1uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glUniform2uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glUniform3uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glUniform4uiv, GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glTexParameterIiv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glTexParameterIuiv, GLenum -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glGetTexParameterIiv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetTexParameterIuiv, GLenum -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glClearBufferiv, GLenum -> GLint -> Ptr GLint -> GL ())
GLAPI(glClearBufferuiv, GLenum -> GLint -> Ptr GLuint -> GL ())
GLAPI(glClearBufferfv, GLenum -> GLint -> Ptr GLfloat -> GL ())
GLAPI(glClearBufferfi, GLenum -> GLint -> GLfloat -> GLint -> GL ())
GLAPI(glGetStringi, GLenum -> GLuint -> GL CString)
GLAPI(glIsRenderbuffer, GLuint -> GL GLboolean)
GLAPI(glBindRenderbuffer, GLenum -> GLuint -> GL ())
GLAPI(glDeleteRenderbuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGenRenderbuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glRenderbufferStorage, GLenum -> GLenum -> GLsizei -> GLsizei -> GL ())
GLAPI(glGetRenderbufferParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glIsFramebuffer, GLuint -> GL GLboolean)
GLAPI(glBindFramebuffer, GLenum -> GLuint -> GL ())
GLAPI(glDeleteFramebuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGenFramebuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glCheckFramebufferStatus, GLenum -> GL GLenum)
GLAPI(glFramebufferTexture1D, GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GL ())
GLAPI(glFramebufferTexture2D, GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GL ())
GLAPI(glFramebufferTexture3D, GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GL ())
GLAPI(glFramebufferRenderbuffer, GLenum -> GLenum -> GLenum -> GLuint -> GL ())
GLAPI(glGetFramebufferAttachmentParameteriv, GLenum -> GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGenerateMipmap, GLenum -> GL ())
GLAPI(glBlitFramebuffer, GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> GL ())
GLAPI(glRenderbufferStorageMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GL ())
GLAPI(glFramebufferTextureLayer, GLenum -> GLenum -> GLuint -> GLint -> GLint -> GL ())
GLAPI(glMapBufferRange, GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> GL (Ptr ()))
GLAPI(glFlushMappedBufferRange, GLenum -> GLintptr -> GLsizeiptr -> GL ())
GLAPI(glBindVertexArray, GLuint -> GL ())
GLAPI(glDeleteVertexArrays, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGenVertexArrays, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glIsVertexArray, GLuint -> GL GLboolean)
-- #endif /* GL_VERSION_3_0 */
-- #ifndef GL_VERSION_3_1
-- #define GL_VERSION_3_1 1
-- #define GL_SAMPLER_2D_RECT                0x8B63
-- #define GL_SAMPLER_2D_RECT_SHADOW         0x8B64
-- #define GL_SAMPLER_BUFFER                 0x8DC2
-- #define GL_INT_SAMPLER_2D_RECT            0x8DCD
-- #define GL_INT_SAMPLER_BUFFER             0x8DD0
-- #define GL_UNSIGNED_INT_SAMPLER_2D_RECT   0x8DD5
-- #define GL_UNSIGNED_INT_SAMPLER_BUFFER    0x8DD8
-- #define GL_TEXTURE_BUFFER                 0x8C2A
-- #define GL_MAX_TEXTURE_BUFFER_SIZE        0x8C2B
-- #define GL_TEXTURE_BINDING_BUFFER         0x8C2C
-- #define GL_TEXTURE_BUFFER_DATA_STORE_BINDING 0x8C2D
-- #define GL_TEXTURE_RECTANGLE              0x84F5
-- #define GL_TEXTURE_BINDING_RECTANGLE      0x84F6
-- #define GL_PROXY_TEXTURE_RECTANGLE        0x84F7
-- #define GL_MAX_RECTANGLE_TEXTURE_SIZE     0x84F8
-- #define GL_R8_SNORM                       0x8F94
-- #define GL_RG8_SNORM                      0x8F95
-- #define GL_RGB8_SNORM                     0x8F96
-- #define GL_RGBA8_SNORM                    0x8F97
-- #define GL_R16_SNORM                      0x8F98
-- #define GL_RG16_SNORM                     0x8F99
-- #define GL_RGB16_SNORM                    0x8F9A
-- #define GL_RGBA16_SNORM                   0x8F9B
-- #define GL_SIGNED_NORMALIZED              0x8F9C
-- #define GL_PRIMITIVE_RESTART              0x8F9D
-- #define GL_PRIMITIVE_RESTART_INDEX        0x8F9E
-- #define GL_COPY_READ_BUFFER               0x8F36
-- #define GL_COPY_WRITE_BUFFER              0x8F37
-- #define GL_UNIFORM_BUFFER                 0x8A11
-- #define GL_UNIFORM_BUFFER_BINDING         0x8A28
-- #define GL_UNIFORM_BUFFER_START           0x8A29
-- #define GL_UNIFORM_BUFFER_SIZE            0x8A2A
-- #define GL_MAX_VERTEX_UNIFORM_BLOCKS      0x8A2B
-- #define GL_MAX_GEOMETRY_UNIFORM_BLOCKS    0x8A2C
-- #define GL_MAX_FRAGMENT_UNIFORM_BLOCKS    0x8A2D
-- #define GL_MAX_COMBINED_UNIFORM_BLOCKS    0x8A2E
-- #define GL_MAX_UNIFORM_BUFFER_BINDINGS    0x8A2F
-- #define GL_MAX_UNIFORM_BLOCK_SIZE         0x8A30
-- #define GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS 0x8A31
-- #define GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS 0x8A32
-- #define GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS 0x8A33
-- #define GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT 0x8A34
-- #define GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH 0x8A35
-- #define GL_ACTIVE_UNIFORM_BLOCKS          0x8A36
-- #define GL_UNIFORM_TYPE                   0x8A37
-- #define GL_UNIFORM_SIZE                   0x8A38
-- #define GL_UNIFORM_NAME_LENGTH            0x8A39
-- #define GL_UNIFORM_BLOCK_INDEX            0x8A3A
-- #define GL_UNIFORM_OFFSET                 0x8A3B
-- #define GL_UNIFORM_ARRAY_STRIDE           0x8A3C
-- #define GL_UNIFORM_MATRIX_STRIDE          0x8A3D
-- #define GL_UNIFORM_IS_ROW_MAJOR           0x8A3E
-- #define GL_UNIFORM_BLOCK_BINDING          0x8A3F
-- #define GL_UNIFORM_BLOCK_DATA_SIZE        0x8A40
-- #define GL_UNIFORM_BLOCK_NAME_LENGTH      0x8A41
-- #define GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS  0x8A42
-- #define GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES 0x8A43
-- #define GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER 0x8A44
-- #define GL_UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER 0x8A45
-- #define GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER 0x8A46
-- #define GL_INVALID_INDEX                  0xFFFFFFFFu
GLAPI(glDrawArraysInstanced, GLenum -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glDrawElementsInstanced, GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GL ())
GLAPI(glTexBuffer, GLenum -> GLenum -> GLuint -> GL ())
GLAPI(glPrimitiveRestartIndex, GLuint -> GL ())
GLAPI(glCopyBufferSubData, GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> GL ())
GLAPI(glGetUniformIndices, GLuint -> GLsizei -> Ptr CString -> Ptr GLuint -> GL ())
GLAPI(glGetActiveUniformsiv, GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetActiveUniformName, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glGetUniformBlockIndex, GLuint -> CString -> GL GLuint)
GLAPI(glGetActiveUniformBlockiv, GLuint -> GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetActiveUniformBlockName, GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glUniformBlockBinding, GLuint -> GLuint -> GLuint -> GL ())
-- #endif /* GL_VERSION_3_1 */
-- #ifndef GL_VERSION_3_2
-- #define GL_VERSION_3_2 1
-- #ifndef GLEXT_64_TYPES_DEFINED
-- #define GLEXT_64_TYPES_DEFINED
-- #if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
-- #include <inttypes.h>
-- #elif defined(__sun__) || defined(__digital__)
-- #include <inttypes.h>
-- #if defined(__STDC__)
-- #if defined(__arch64__) || defined(_LP64)
-- #else
-- #endif /* __arch64__ */
-- #endif /* __STDC__ */
-- #elif defined( __VMS ) || defined(__sgi)
-- #include <inttypes.h>
-- #elif defined(__SCO__) || defined(__USLC__)
-- #include <stdint.h>
-- #elif defined(__UNIXOS2__) || defined(__SOL64__)
-- #elif defined(_WIN32) && defined(__GNUC__)
-- #include <stdint.h>
-- #elif defined(_WIN32)
-- #else
-- #include <inttypes.h>
-- #define GL_CONTEXT_CORE_PROFILE_BIT       0x00000001
-- #define GL_CONTEXT_COMPATIBILITY_PROFILE_BIT 0x00000002
-- #define GL_LINES_ADJACENCY                0x000A
-- #define GL_LINE_STRIP_ADJACENCY           0x000B
-- #define GL_TRIANGLES_ADJACENCY            0x000C
-- #define GL_TRIANGLE_STRIP_ADJACENCY       0x000D
-- #define GL_PROGRAM_POINT_SIZE             0x8642
-- #define GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS 0x8C29
-- #define GL_FRAMEBUFFER_ATTACHMENT_LAYERED 0x8DA7
-- #define GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS 0x8DA8
-- #define GL_GEOMETRY_SHADER                0x8DD9
-- #define GL_GEOMETRY_VERTICES_OUT          0x8916
-- #define GL_GEOMETRY_INPUT_TYPE            0x8917
-- #define GL_GEOMETRY_OUTPUT_TYPE           0x8918
-- #define GL_MAX_GEOMETRY_UNIFORM_COMPONENTS 0x8DDF
-- #define GL_MAX_GEOMETRY_OUTPUT_VERTICES   0x8DE0
-- #define GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS 0x8DE1
-- #define GL_MAX_VERTEX_OUTPUT_COMPONENTS   0x9122
-- #define GL_MAX_GEOMETRY_INPUT_COMPONENTS  0x9123
-- #define GL_MAX_GEOMETRY_OUTPUT_COMPONENTS 0x9124
-- #define GL_MAX_FRAGMENT_INPUT_COMPONENTS  0x9125
-- #define GL_CONTEXT_PROFILE_MASK           0x9126
-- #define GL_DEPTH_CLAMP                    0x864F
-- #define GL_QUADS_FOLLOW_PROVOKING_VERTEX_CONVENTION 0x8E4C
-- #define GL_FIRST_VERTEX_CONVENTION        0x8E4D
-- #define GL_LAST_VERTEX_CONVENTION         0x8E4E
-- #define GL_PROVOKING_VERTEX               0x8E4F
-- #define GL_TEXTURE_CUBE_MAP_SEAMLESS      0x884F
-- #define GL_MAX_SERVER_WAIT_TIMEOUT        0x9111
-- #define GL_OBJECT_TYPE                    0x9112
-- #define GL_SYNC_CONDITION                 0x9113
-- #define GL_SYNC_STATUS                    0x9114
-- #define GL_SYNC_FLAGS                     0x9115
-- #define GL_SYNC_FENCE                     0x9116
-- #define GL_SYNC_GPU_COMMANDS_COMPLETE     0x9117
-- #define GL_UNSIGNALED                     0x9118
-- #define GL_SIGNALED                       0x9119
-- #define GL_ALREADY_SIGNALED               0x911A
-- #define GL_TIMEOUT_EXPIRED                0x911B
-- #define GL_CONDITION_SATISFIED            0x911C
-- #define GL_WAIT_FAILED                    0x911D
-- #define GL_TIMEOUT_IGNORED                0xFFFFFFFFFFFFFFFFull
-- #define GL_SYNC_FLUSH_COMMANDS_BIT        0x00000001
-- #define GL_SAMPLE_POSITION                0x8E50
-- #define GL_SAMPLE_MASK                    0x8E51
-- #define GL_SAMPLE_MASK_VALUE              0x8E52
-- #define GL_MAX_SAMPLE_MASK_WORDS          0x8E59
-- #define GL_TEXTURE_2D_MULTISAMPLE         0x9100
-- #define GL_PROXY_TEXTURE_2D_MULTISAMPLE   0x9101
-- #define GL_TEXTURE_2D_MULTISAMPLE_ARRAY   0x9102
-- #define GL_PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY 0x9103
-- #define GL_TEXTURE_BINDING_2D_MULTISAMPLE 0x9104
-- #define GL_TEXTURE_BINDING_2D_MULTISAMPLE_ARRAY 0x9105
-- #define GL_TEXTURE_SAMPLES                0x9106
-- #define GL_TEXTURE_FIXED_SAMPLE_LOCATIONS 0x9107
-- #define GL_SAMPLER_2D_MULTISAMPLE         0x9108
-- #define GL_INT_SAMPLER_2D_MULTISAMPLE     0x9109
-- #define GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE 0x910A
-- #define GL_SAMPLER_2D_MULTISAMPLE_ARRAY   0x910B
-- #define GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY 0x910C
-- #define GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY 0x910D
-- #define GL_MAX_COLOR_TEXTURE_SAMPLES      0x910E
-- #define GL_MAX_DEPTH_TEXTURE_SAMPLES      0x910F
-- #define GL_MAX_INTEGER_SAMPLES            0x9110
GLAPI(glDrawElementsBaseVertex, GLenum -> GLsizei -> GLenum -> Ptr () -> GLint -> GL ())
GLAPI(glDrawRangeElementsBaseVertex, GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr () -> GLint -> GL ())
GLAPI(glDrawElementsInstancedBaseVertex, GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GL ())
GLAPI(glMultiDrawElementsBaseVertex, GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr ()) -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glProvokingVertex, GLenum -> GL ())
GLAPI(glFenceSync, GLenum -> GLbitfield -> GL GLsync)
GLAPI(glIsSync, GLsync -> GL GLboolean)
GLAPI(glDeleteSync, GLsync -> GL ())
GLAPI(glClientWaitSync, GLsync -> GLbitfield -> GLuint64 -> GL GLenum)
GLAPI(glWaitSync, GLsync -> GLbitfield -> GLuint64 -> GL ())
GLAPI(glGetInteger64v, GLenum -> Ptr GLint64 -> GL ())
GLAPI(glGetSynciv, GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> GL ())
GLAPI(glGetInteger64i_v, GLenum -> GLuint -> Ptr GLint64 -> GL ())
GLAPI(glGetBufferParameteri64v, GLenum -> GLenum -> Ptr GLint64 -> GL ())
GLAPI(glFramebufferTexture, GLenum -> GLenum -> GLuint -> GLint -> GL ())
GLAPI(glTexImage2DMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> GL ())
GLAPI(glTexImage3DMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> GL ())
GLAPI(glGetMultisamplefv, GLenum -> GLuint -> Ptr GLfloat -> GL ())
GLAPI(glSampleMaski, GLuint -> GLbitfield -> GL ())
-- #endif /* GL_VERSION_3_2 */
-- #ifndef GL_VERSION_3_3
-- #define GL_VERSION_3_3 1
-- #define GL_VERTEX_ATTRIB_ARRAY_DIVISOR    0x88FE
-- #define GL_SRC1_COLOR                     0x88F9
-- #define GL_ONE_MINUS_SRC1_COLOR           0x88FA
-- #define GL_ONE_MINUS_SRC1_ALPHA           0x88FB
-- #define GL_MAX_DUAL_SOURCE_DRAW_BUFFERS   0x88FC
-- #define GL_ANY_SAMPLES_PASSED             0x8C2F
-- #define GL_SAMPLER_BINDING                0x8919
-- #define GL_RGB10_A2UI                     0x906F
-- #define GL_TEXTURE_SWIZZLE_R              0x8E42
-- #define GL_TEXTURE_SWIZZLE_G              0x8E43
-- #define GL_TEXTURE_SWIZZLE_B              0x8E44
-- #define GL_TEXTURE_SWIZZLE_A              0x8E45
-- #define GL_TEXTURE_SWIZZLE_RGBA           0x8E46
-- #define GL_TIME_ELAPSED                   0x88BF
-- #define GL_TIMESTAMP                      0x8E28
-- #define GL_INT_2_10_10_10_REV             0x8D9F
GLAPI(glBindFragDataLocationIndexed, GLuint -> GLuint -> GLuint -> CString -> GL ())
GLAPI(glGetFragDataIndex, GLuint -> CString -> GL GLint)
GLAPI(glGenSamplers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glDeleteSamplers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glIsSampler, GLuint -> GL GLboolean)
GLAPI(glBindSampler, GLuint -> GLuint -> GL ())
GLAPI(glSamplerParameteri, GLuint -> GLenum -> GLint -> GL ())
GLAPI(glSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glSamplerParameterf, GLuint -> GLenum -> GLfloat -> GL ())
GLAPI(glSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glSamplerParameterIiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glSamplerParameterIuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glGetSamplerParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetSamplerParameterIiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetSamplerParameterfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glGetSamplerParameterIuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glQueryCounter, GLuint -> GLenum -> GL ())
GLAPI(glGetQueryObjecti64v, GLuint -> GLenum -> Ptr GLint64 -> GL ())
GLAPI(glGetQueryObjectui64v, GLuint -> GLenum -> Ptr GLuint64 -> GL ())
GLAPI(glVertexAttribDivisor, GLuint -> GLuint -> GL ())
GLAPI(glVertexAttribP1ui, GLuint -> GLenum -> GLboolean -> GLuint -> GL ())
GLAPI(glVertexAttribP1uiv, GLuint -> GLenum -> GLboolean -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribP2ui, GLuint -> GLenum -> GLboolean -> GLuint -> GL ())
GLAPI(glVertexAttribP2uiv, GLuint -> GLenum -> GLboolean -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribP3ui, GLuint -> GLenum -> GLboolean -> GLuint -> GL ())
GLAPI(glVertexAttribP3uiv, GLuint -> GLenum -> GLboolean -> Ptr GLuint -> GL ())
GLAPI(glVertexAttribP4ui, GLuint -> GLenum -> GLboolean -> GLuint -> GL ())
GLAPI(glVertexAttribP4uiv, GLuint -> GLenum -> GLboolean -> Ptr GLuint -> GL ())
-- #endif /* GL_VERSION_3_3 */
-- #ifndef GL_VERSION_4_0
-- #define GL_VERSION_4_0 1
-- #define GL_SAMPLE_SHADING                 0x8C36
-- #define GL_MIN_SAMPLE_SHADING_VALUE       0x8C37
-- #define GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET 0x8E5E
-- #define GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET 0x8E5F
-- #define GL_TEXTURE_CUBE_MAP_ARRAY         0x9009
-- #define GL_TEXTURE_BINDING_CUBE_MAP_ARRAY 0x900A
-- #define GL_PROXY_TEXTURE_CUBE_MAP_ARRAY   0x900B
-- #define GL_SAMPLER_CUBE_MAP_ARRAY         0x900C
-- #define GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW  0x900D
-- #define GL_INT_SAMPLER_CUBE_MAP_ARRAY     0x900E
-- #define GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY 0x900F
-- #define GL_DRAW_INDIRECT_BUFFER           0x8F3F
-- #define GL_DRAW_INDIRECT_BUFFER_BINDING   0x8F43
-- #define GL_GEOMETRY_SHADER_INVOCATIONS    0x887F
-- #define GL_MAX_GEOMETRY_SHADER_INVOCATIONS 0x8E5A
-- #define GL_MIN_FRAGMENT_INTERPOLATION_OFFSET 0x8E5B
-- #define GL_MAX_FRAGMENT_INTERPOLATION_OFFSET 0x8E5C
-- #define GL_FRAGMENT_INTERPOLATION_OFFSET_BITS 0x8E5D
-- #define GL_MAX_VERTEX_STREAMS             0x8E71
-- #define GL_DOUBLE_VEC2                    0x8FFC
-- #define GL_DOUBLE_VEC3                    0x8FFD
-- #define GL_DOUBLE_VEC4                    0x8FFE
-- #define GL_DOUBLE_MAT2                    0x8F46
-- #define GL_DOUBLE_MAT3                    0x8F47
-- #define GL_DOUBLE_MAT4                    0x8F48
-- #define GL_DOUBLE_MAT2x3                  0x8F49
-- #define GL_DOUBLE_MAT2x4                  0x8F4A
-- #define GL_DOUBLE_MAT3x2                  0x8F4B
-- #define GL_DOUBLE_MAT3x4                  0x8F4C
-- #define GL_DOUBLE_MAT4x2                  0x8F4D
-- #define GL_DOUBLE_MAT4x3                  0x8F4E
-- #define GL_ACTIVE_SUBROUTINES             0x8DE5
-- #define GL_ACTIVE_SUBROUTINE_UNIFORMS     0x8DE6
-- #define GL_ACTIVE_SUBROUTINE_UNIFORM_LOCATIONS 0x8E47
-- #define GL_ACTIVE_SUBROUTINE_MAX_LENGTH   0x8E48
-- #define GL_ACTIVE_SUBROUTINE_UNIFORM_MAX_LENGTH 0x8E49
-- #define GL_MAX_SUBROUTINES                0x8DE7
-- #define GL_MAX_SUBROUTINE_UNIFORM_LOCATIONS 0x8DE8
-- #define GL_NUM_COMPATIBLE_SUBROUTINES     0x8E4A
-- #define GL_COMPATIBLE_SUBROUTINES         0x8E4B
-- #define GL_PATCHES                        0x000E
-- #define GL_PATCH_VERTICES                 0x8E72
-- #define GL_PATCH_DEFAULT_INNER_LEVEL      0x8E73
-- #define GL_PATCH_DEFAULT_OUTER_LEVEL      0x8E74
-- #define GL_TESS_CONTROL_OUTPUT_VERTICES   0x8E75
-- #define GL_TESS_GEN_MODE                  0x8E76
-- #define GL_TESS_GEN_SPACING               0x8E77
-- #define GL_TESS_GEN_VERTEX_ORDER          0x8E78
-- #define GL_TESS_GEN_POINT_MODE            0x8E79
-- #define GL_ISOLINES                       0x8E7A
-- #define GL_FRACTIONAL_ODD                 0x8E7B
-- #define GL_FRACTIONAL_EVEN                0x8E7C
-- #define GL_MAX_PATCH_VERTICES             0x8E7D
-- #define GL_MAX_TESS_GEN_LEVEL             0x8E7E
-- #define GL_MAX_TESS_CONTROL_UNIFORM_COMPONENTS 0x8E7F
-- #define GL_MAX_TESS_EVALUATION_UNIFORM_COMPONENTS 0x8E80
-- #define GL_MAX_TESS_CONTROL_TEXTURE_IMAGE_UNITS 0x8E81
-- #define GL_MAX_TESS_EVALUATION_TEXTURE_IMAGE_UNITS 0x8E82
-- #define GL_MAX_TESS_CONTROL_OUTPUT_COMPONENTS 0x8E83
-- #define GL_MAX_TESS_PATCH_COMPONENTS      0x8E84
-- #define GL_MAX_TESS_CONTROL_TOTAL_OUTPUT_COMPONENTS 0x8E85
-- #define GL_MAX_TESS_EVALUATION_OUTPUT_COMPONENTS 0x8E86
-- #define GL_MAX_TESS_CONTROL_UNIFORM_BLOCKS 0x8E89
-- #define GL_MAX_TESS_EVALUATION_UNIFORM_BLOCKS 0x8E8A
-- #define GL_MAX_TESS_CONTROL_INPUT_COMPONENTS 0x886C
-- #define GL_MAX_TESS_EVALUATION_INPUT_COMPONENTS 0x886D
-- #define GL_MAX_COMBINED_TESS_CONTROL_UNIFORM_COMPONENTS 0x8E1E
-- #define GL_MAX_COMBINED_TESS_EVALUATION_UNIFORM_COMPONENTS 0x8E1F
-- #define GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_CONTROL_SHADER 0x84F0
-- #define GL_UNIFORM_BLOCK_REFERENCED_BY_TESS_EVALUATION_SHADER 0x84F1
-- #define GL_TESS_EVALUATION_SHADER         0x8E87
-- #define GL_TESS_CONTROL_SHADER            0x8E88
-- #define GL_TRANSFORM_FEEDBACK             0x8E22
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_PAUSED 0x8E23
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_ACTIVE 0x8E24
-- #define GL_TRANSFORM_FEEDBACK_BINDING     0x8E25
-- #define GL_MAX_TRANSFORM_FEEDBACK_BUFFERS 0x8E70
GLAPI(glMinSampleShading, GLfloat -> GL ())
GLAPI(glBlendEquationi, GLuint -> GLenum -> GL ())
GLAPI(glBlendEquationSeparatei, GLuint -> GLenum -> GLenum -> GL ())
GLAPI(glBlendFunci, GLuint -> GLenum -> GLenum -> GL ())
GLAPI(glBlendFuncSeparatei, GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> GL ())
GLAPI(glDrawArraysIndirect, GLenum -> Ptr () -> GL ())
GLAPI(glDrawElementsIndirect, GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glUniform1d, GLint -> GLdouble -> GL ())
GLAPI(glUniform2d, GLint -> GLdouble -> GLdouble -> GL ())
GLAPI(glUniform3d, GLint -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glUniform4d, GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glUniform1dv, GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glUniform2dv, GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glUniform3dv, GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glUniform4dv, GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix2dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix3dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix4dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix2x3dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix2x4dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix3x2dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix3x4dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix4x2dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glUniformMatrix4x3dv, GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glGetUniformdv, GLuint -> GLint -> Ptr GLdouble -> GL ())
GLAPI(glGetSubroutineUniformLocation, GLuint -> GLenum -> CString -> GL GLint)
GLAPI(glGetSubroutineIndex, GLuint -> GLenum -> CString -> GL GLuint)
GLAPI(glGetActiveSubroutineUniformiv, GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetActiveSubroutineUniformName, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glGetActiveSubroutineName, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glUniformSubroutinesuiv, GLenum -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGetUniformSubroutineuiv, GLenum -> GLint -> Ptr GLuint -> GL ())
GLAPI(glGetProgramStageiv, GLuint -> GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glPatchParameteri, GLenum -> GLint -> GL ())
GLAPI(glPatchParameterfv, GLenum -> Ptr GLfloat -> GL ())
GLAPI(glBindTransformFeedback, GLenum -> GLuint -> GL ())
GLAPI(glDeleteTransformFeedbacks, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGenTransformFeedbacks, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glIsTransformFeedback, GLuint -> GL GLboolean)
GLAPI(glPauseTransformFeedback, GL ())
GLAPI(glResumeTransformFeedback, GL ())
GLAPI(glDrawTransformFeedback, GLenum -> GLuint -> GL ())
GLAPI(glDrawTransformFeedbackStream, GLenum -> GLuint -> GLuint -> GL ())
GLAPI(glBeginQueryIndexed, GLenum -> GLuint -> GLuint -> GL ())
GLAPI(glEndQueryIndexed, GLenum -> GLuint -> GL ())
GLAPI(glGetQueryIndexediv, GLenum -> GLuint -> GLenum -> Ptr GLint -> GL ())
-- #endif /* GL_VERSION_4_0 */
-- #ifndef GL_VERSION_4_1
-- #define GL_VERSION_4_1 1
-- #define GL_FIXED                          0x140C
-- #define GL_IMPLEMENTATION_COLOR_READ_TYPE 0x8B9A
-- #define GL_IMPLEMENTATION_COLOR_READ_FORMAT 0x8B9B
-- #define GL_LOW_FLOAT                      0x8DF0
-- #define GL_MEDIUM_FLOAT                   0x8DF1
-- #define GL_HIGH_FLOAT                     0x8DF2
-- #define GL_LOW_INT                        0x8DF3
-- #define GL_MEDIUM_INT                     0x8DF4
-- #define GL_HIGH_INT                       0x8DF5
-- #define GL_SHADER_COMPILER                0x8DFA
-- #define GL_SHADER_BINARY_FORMATS          0x8DF8
-- #define GL_NUM_SHADER_BINARY_FORMATS      0x8DF9
-- #define GL_MAX_VERTEX_UNIFORM_VECTORS     0x8DFB
-- #define GL_MAX_VARYING_VECTORS            0x8DFC
-- #define GL_MAX_FRAGMENT_UNIFORM_VECTORS   0x8DFD
-- #define GL_RGB565                         0x8D62
-- #define GL_PROGRAM_BINARY_RETRIEVABLE_HINT 0x8257
-- #define GL_PROGRAM_BINARY_LENGTH          0x8741
-- #define GL_NUM_PROGRAM_BINARY_FORMATS     0x87FE
-- #define GL_PROGRAM_BINARY_FORMATS         0x87FF
-- #define GL_VERTEX_SHADER_BIT              0x00000001
-- #define GL_FRAGMENT_SHADER_BIT            0x00000002
-- #define GL_GEOMETRY_SHADER_BIT            0x00000004
-- #define GL_TESS_CONTROL_SHADER_BIT        0x00000008
-- #define GL_TESS_EVALUATION_SHADER_BIT     0x00000010
-- #define GL_ALL_SHADER_BITS                0xFFFFFFFF
-- #define GL_PROGRAM_SEPARABLE              0x8258
-- #define GL_ACTIVE_PROGRAM                 0x8259
-- #define GL_PROGRAM_PIPELINE_BINDING       0x825A
-- #define GL_MAX_VIEWPORTS                  0x825B
-- #define GL_VIEWPORT_SUBPIXEL_BITS         0x825C
-- #define GL_VIEWPORT_BOUNDS_RANGE          0x825D
-- #define GL_LAYER_PROVOKING_VERTEX         0x825E
-- #define GL_VIEWPORT_INDEX_PROVOKING_VERTEX 0x825F
-- #define GL_UNDEFINED_VERTEX               0x8260
GLAPI(glReleaseShaderCompiler, GL ())
GLAPI(glShaderBinary, GLsizei -> Ptr GLuint -> GLenum -> Ptr () -> GLsizei -> GL ())
GLAPI(glGetShaderPrecisionFormat, GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> GL ())
GLAPI(glDepthRangef, GLfloat -> GLfloat -> GL ())
GLAPI(glClearDepthf, GLfloat -> GL ())
GLAPI(glGetProgramBinary, GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr () -> GL ())
GLAPI(glProgramBinary, GLuint -> GLenum -> Ptr () -> GLsizei -> GL ())
GLAPI(glProgramParameteri, GLuint -> GLenum -> GLint -> GL ())
GLAPI(glUseProgramStages, GLuint -> GLbitfield -> GLuint -> GL ())
GLAPI(glActiveShaderProgram, GLuint -> GLuint -> GL ())
GLAPI(glCreateShaderProgramv, GLenum -> GLsizei -> Ptr CString -> GL GLuint)
GLAPI(glBindProgramPipeline, GLuint -> GL ())
GLAPI(glDeleteProgramPipelines, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGenProgramPipelines, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glIsProgramPipeline, GLuint -> GL GLboolean)
GLAPI(glGetProgramPipelineiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glProgramUniform1i, GLuint -> GLint -> GLint -> GL ())
GLAPI(glProgramUniform1iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glProgramUniform1f, GLuint -> GLint -> GLfloat -> GL ())
GLAPI(glProgramUniform1fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniform1d, GLuint -> GLint -> GLdouble -> GL ())
GLAPI(glProgramUniform1dv, GLuint -> GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniform1ui, GLuint -> GLint -> GLuint -> GL ())
GLAPI(glProgramUniform1uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glProgramUniform2i, GLuint -> GLint -> GLint -> GLint -> GL ())
GLAPI(glProgramUniform2iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glProgramUniform2f, GLuint -> GLint -> GLfloat -> GLfloat -> GL ())
GLAPI(glProgramUniform2fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniform2d, GLuint -> GLint -> GLdouble -> GLdouble -> GL ())
GLAPI(glProgramUniform2dv, GLuint -> GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniform2ui, GLuint -> GLint -> GLuint -> GLuint -> GL ())
GLAPI(glProgramUniform2uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glProgramUniform3i, GLuint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLAPI(glProgramUniform3iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glProgramUniform3f, GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glProgramUniform3fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniform3d, GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glProgramUniform3dv, GLuint -> GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniform3ui, GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glProgramUniform3uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glProgramUniform4i, GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GL ())
GLAPI(glProgramUniform4iv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glProgramUniform4f, GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glProgramUniform4fv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniform4d, GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glProgramUniform4dv, GLuint -> GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniform4ui, GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glProgramUniform4uiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glProgramUniformMatrix2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix2dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix3dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix4dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix2x3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix3x2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix2x4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix4x2fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix3x4fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix4x3fv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> GL ())
GLAPI(glProgramUniformMatrix2x3dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix3x2dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix2x4dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix4x2dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix3x4dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glProgramUniformMatrix4x3dv, GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> GL ())
GLAPI(glValidateProgramPipeline, GLuint -> GL ())
GLAPI(glGetProgramPipelineInfoLog, GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glVertexAttribL1d, GLuint -> GLdouble -> GL ())
GLAPI(glVertexAttribL2d, GLuint -> GLdouble -> GLdouble -> GL ())
GLAPI(glVertexAttribL3d, GLuint -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glVertexAttribL4d, GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GL ())
GLAPI(glVertexAttribL1dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttribL2dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttribL3dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttribL4dv, GLuint -> Ptr GLdouble -> GL ())
GLAPI(glVertexAttribLPointer, GLuint -> GLint -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetVertexAttribLdv, GLuint -> GLenum -> Ptr GLdouble -> GL ())
GLAPI(glViewportArrayv, GLuint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glViewportIndexedf, GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL ())
GLAPI(glViewportIndexedfv, GLuint -> Ptr GLfloat -> GL ())
GLAPI(glScissorArrayv, GLuint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glScissorIndexed, GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glScissorIndexedv, GLuint -> Ptr GLint -> GL ())
GLAPI(glDepthRangeArrayv, GLuint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glDepthRangeIndexed, GLuint -> GLdouble -> GLdouble -> GL ())
GLAPI(glGetFloati_v, GLenum -> GLuint -> Ptr GLfloat -> GL ())
GLAPI(glGetDoublei_v, GLenum -> GLuint -> Ptr GLdouble -> GL ())
-- #endif /* GL_VERSION_4_1 */
-- #ifndef GL_VERSION_4_2
-- #define GL_VERSION_4_2 1
-- #define GL_COPY_READ_BUFFER_BINDING       0x8F36
-- #define GL_COPY_WRITE_BUFFER_BINDING      0x8F37
-- #define GL_TRANSFORM_FEEDBACK_ACTIVE      0x8E24
-- #define GL_TRANSFORM_FEEDBACK_PAUSED      0x8E23
-- #define GL_UNPACK_COMPRESSED_BLOCK_WIDTH  0x9127
-- #define GL_UNPACK_COMPRESSED_BLOCK_HEIGHT 0x9128
-- #define GL_UNPACK_COMPRESSED_BLOCK_DEPTH  0x9129
-- #define GL_UNPACK_COMPRESSED_BLOCK_SIZE   0x912A
-- #define GL_PACK_COMPRESSED_BLOCK_WIDTH    0x912B
-- #define GL_PACK_COMPRESSED_BLOCK_HEIGHT   0x912C
-- #define GL_PACK_COMPRESSED_BLOCK_DEPTH    0x912D
-- #define GL_PACK_COMPRESSED_BLOCK_SIZE     0x912E
-- #define GL_NUM_SAMPLE_COUNTS              0x9380
-- #define GL_MIN_MAP_BUFFER_ALIGNMENT       0x90BC
-- #define GL_ATOMIC_COUNTER_BUFFER          0x92C0
-- #define GL_ATOMIC_COUNTER_BUFFER_BINDING  0x92C1
-- #define GL_ATOMIC_COUNTER_BUFFER_START    0x92C2
-- #define GL_ATOMIC_COUNTER_BUFFER_SIZE     0x92C3
-- #define GL_ATOMIC_COUNTER_BUFFER_DATA_SIZE 0x92C4
-- #define GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTERS 0x92C5
-- #define GL_ATOMIC_COUNTER_BUFFER_ACTIVE_ATOMIC_COUNTER_INDICES 0x92C6
-- #define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_VERTEX_SHADER 0x92C7
-- #define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_CONTROL_SHADER 0x92C8
-- #define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_TESS_EVALUATION_SHADER 0x92C9
-- #define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_GEOMETRY_SHADER 0x92CA
-- #define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_FRAGMENT_SHADER 0x92CB
-- #define GL_MAX_VERTEX_ATOMIC_COUNTER_BUFFERS 0x92CC
-- #define GL_MAX_TESS_CONTROL_ATOMIC_COUNTER_BUFFERS 0x92CD
-- #define GL_MAX_TESS_EVALUATION_ATOMIC_COUNTER_BUFFERS 0x92CE
-- #define GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS 0x92CF
-- #define GL_MAX_FRAGMENT_ATOMIC_COUNTER_BUFFERS 0x92D0
-- #define GL_MAX_COMBINED_ATOMIC_COUNTER_BUFFERS 0x92D1
-- #define GL_MAX_VERTEX_ATOMIC_COUNTERS     0x92D2
-- #define GL_MAX_TESS_CONTROL_ATOMIC_COUNTERS 0x92D3
-- #define GL_MAX_TESS_EVALUATION_ATOMIC_COUNTERS 0x92D4
-- #define GL_MAX_GEOMETRY_ATOMIC_COUNTERS   0x92D5
-- #define GL_MAX_FRAGMENT_ATOMIC_COUNTERS   0x92D6
-- #define GL_MAX_COMBINED_ATOMIC_COUNTERS   0x92D7
-- #define GL_MAX_ATOMIC_COUNTER_BUFFER_SIZE 0x92D8
-- #define GL_MAX_ATOMIC_COUNTER_BUFFER_BINDINGS 0x92DC
-- #define GL_ACTIVE_ATOMIC_COUNTER_BUFFERS  0x92D9
-- #define GL_UNIFORM_ATOMIC_COUNTER_BUFFER_INDEX 0x92DA
-- #define GL_UNSIGNED_INT_ATOMIC_COUNTER    0x92DB
-- #define GL_VERTEX_ATTRIB_ARRAY_BARRIER_BIT 0x00000001
-- #define GL_ELEMENT_ARRAY_BARRIER_BIT      0x00000002
-- #define GL_UNIFORM_BARRIER_BIT            0x00000004
-- #define GL_TEXTURE_FETCH_BARRIER_BIT      0x00000008
-- #define GL_SHADER_IMAGE_ACCESS_BARRIER_BIT 0x00000020
-- #define GL_COMMAND_BARRIER_BIT            0x00000040
-- #define GL_PIXEL_BUFFER_BARRIER_BIT       0x00000080
-- #define GL_TEXTURE_UPDATE_BARRIER_BIT     0x00000100
-- #define GL_BUFFER_UPDATE_BARRIER_BIT      0x00000200
-- #define GL_FRAMEBUFFER_BARRIER_BIT        0x00000400
-- #define GL_TRANSFORM_FEEDBACK_BARRIER_BIT 0x00000800
-- #define GL_ATOMIC_COUNTER_BARRIER_BIT     0x00001000
-- #define GL_ALL_BARRIER_BITS               0xFFFFFFFF
-- #define GL_MAX_IMAGE_UNITS                0x8F38
-- #define GL_MAX_COMBINED_IMAGE_UNITS_AND_FRAGMENT_OUTPUTS 0x8F39
-- #define GL_IMAGE_BINDING_NAME             0x8F3A
-- #define GL_IMAGE_BINDING_LEVEL            0x8F3B
-- #define GL_IMAGE_BINDING_LAYERED          0x8F3C
-- #define GL_IMAGE_BINDING_LAYER            0x8F3D
-- #define GL_IMAGE_BINDING_ACCESS           0x8F3E
-- #define GL_IMAGE_1D                       0x904C
-- #define GL_IMAGE_2D                       0x904D
-- #define GL_IMAGE_3D                       0x904E
-- #define GL_IMAGE_2D_RECT                  0x904F
-- #define GL_IMAGE_CUBE                     0x9050
-- #define GL_IMAGE_BUFFER                   0x9051
-- #define GL_IMAGE_1D_ARRAY                 0x9052
-- #define GL_IMAGE_2D_ARRAY                 0x9053
-- #define GL_IMAGE_CUBE_MAP_ARRAY           0x9054
-- #define GL_IMAGE_2D_MULTISAMPLE           0x9055
-- #define GL_IMAGE_2D_MULTISAMPLE_ARRAY     0x9056
-- #define GL_INT_IMAGE_1D                   0x9057
-- #define GL_INT_IMAGE_2D                   0x9058
-- #define GL_INT_IMAGE_3D                   0x9059
-- #define GL_INT_IMAGE_2D_RECT              0x905A
-- #define GL_INT_IMAGE_CUBE                 0x905B
-- #define GL_INT_IMAGE_BUFFER               0x905C
-- #define GL_INT_IMAGE_1D_ARRAY             0x905D
-- #define GL_INT_IMAGE_2D_ARRAY             0x905E
-- #define GL_INT_IMAGE_CUBE_MAP_ARRAY       0x905F
-- #define GL_INT_IMAGE_2D_MULTISAMPLE       0x9060
-- #define GL_INT_IMAGE_2D_MULTISAMPLE_ARRAY 0x9061
-- #define GL_UNSIGNED_INT_IMAGE_1D          0x9062
-- #define GL_UNSIGNED_INT_IMAGE_2D          0x9063
-- #define GL_UNSIGNED_INT_IMAGE_3D          0x9064
-- #define GL_UNSIGNED_INT_IMAGE_2D_RECT     0x9065
-- #define GL_UNSIGNED_INT_IMAGE_CUBE        0x9066
-- #define GL_UNSIGNED_INT_IMAGE_BUFFER      0x9067
-- #define GL_UNSIGNED_INT_IMAGE_1D_ARRAY    0x9068
-- #define GL_UNSIGNED_INT_IMAGE_2D_ARRAY    0x9069
-- #define GL_UNSIGNED_INT_IMAGE_CUBE_MAP_ARRAY 0x906A
-- #define GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE 0x906B
-- #define GL_UNSIGNED_INT_IMAGE_2D_MULTISAMPLE_ARRAY 0x906C
-- #define GL_MAX_IMAGE_SAMPLES              0x906D
-- #define GL_IMAGE_BINDING_FORMAT           0x906E
-- #define GL_IMAGE_FORMAT_COMPATIBILITY_TYPE 0x90C7
-- #define GL_IMAGE_FORMAT_COMPATIBILITY_BY_SIZE 0x90C8
-- #define GL_IMAGE_FORMAT_COMPATIBILITY_BY_CLASS 0x90C9
-- #define GL_MAX_VERTEX_IMAGE_UNIFORMS      0x90CA
-- #define GL_MAX_TESS_CONTROL_IMAGE_UNIFORMS 0x90CB
-- #define GL_MAX_TESS_EVALUATION_IMAGE_UNIFORMS 0x90CC
-- #define GL_MAX_GEOMETRY_IMAGE_UNIFORMS    0x90CD
-- #define GL_MAX_FRAGMENT_IMAGE_UNIFORMS    0x90CE
-- #define GL_MAX_COMBINED_IMAGE_UNIFORMS    0x90CF
-- #define GL_COMPRESSED_RGBA_BPTC_UNORM     0x8E8C
-- #define GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM 0x8E8D
-- #define GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT 0x8E8E
-- #define GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT 0x8E8F
-- #define GL_TEXTURE_IMMUTABLE_FORMAT       0x912F
GLAPI(glDrawArraysInstancedBaseInstance, GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> GL ())
GLAPI(glDrawElementsInstancedBaseInstance, GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLuint -> GL ())
GLAPI(glDrawElementsInstancedBaseVertexBaseInstance, GLenum -> GLsizei -> GLenum -> Ptr () -> GLsizei -> GLint -> GLuint -> GL ())
GLAPI(glGetInternalformativ, GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glGetActiveAtomicCounterBufferiv, GLuint -> GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glBindImageTexture, GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> GL ())
GLAPI(glMemoryBarrier, GLbitfield -> GL ())
GLAPI(glTexStorage1D, GLenum -> GLsizei -> GLenum -> GLsizei -> GL ())
GLAPI(glTexStorage2D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GL ())
GLAPI(glTexStorage3D, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GL ())
GLAPI(glDrawTransformFeedbackInstanced, GLenum -> GLuint -> GLsizei -> GL ())
GLAPI(glDrawTransformFeedbackStreamInstanced, GLenum -> GLuint -> GLuint -> GLsizei -> GL ())
-- #endif /* GL_VERSION_4_2 */
-- #ifndef GL_VERSION_4_3
-- #define GL_VERSION_4_3 1
-- #define GL_NUM_SHADING_LANGUAGE_VERSIONS  0x82E9
-- #define GL_VERTEX_ATTRIB_ARRAY_LONG       0x874E
-- #define GL_COMPRESSED_RGB8_ETC2           0x9274
-- #define GL_COMPRESSED_SRGB8_ETC2          0x9275
-- #define GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 0x9276
-- #define GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 0x9277
-- #define GL_COMPRESSED_RGBA8_ETC2_EAC      0x9278
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC 0x9279
-- #define GL_COMPRESSED_R11_EAC             0x9270
-- #define GL_COMPRESSED_SIGNED_R11_EAC      0x9271
-- #define GL_COMPRESSED_RG11_EAC            0x9272
-- #define GL_COMPRESSED_SIGNED_RG11_EAC     0x9273
-- #define GL_PRIMITIVE_RESTART_FIXED_INDEX  0x8D69
-- #define GL_ANY_SAMPLES_PASSED_CONSERVATIVE 0x8D6A
-- #define GL_MAX_ELEMENT_INDEX              0x8D6B
-- #define GL_COMPUTE_SHADER                 0x91B9
-- #define GL_MAX_COMPUTE_UNIFORM_BLOCKS     0x91BB
-- #define GL_MAX_COMPUTE_TEXTURE_IMAGE_UNITS 0x91BC
-- #define GL_MAX_COMPUTE_IMAGE_UNIFORMS     0x91BD
-- #define GL_MAX_COMPUTE_SHARED_MEMORY_SIZE 0x8262
-- #define GL_MAX_COMPUTE_UNIFORM_COMPONENTS 0x8263
-- #define GL_MAX_COMPUTE_ATOMIC_COUNTER_BUFFERS 0x8264
-- #define GL_MAX_COMPUTE_ATOMIC_COUNTERS    0x8265
-- #define GL_MAX_COMBINED_COMPUTE_UNIFORM_COMPONENTS 0x8266
-- #define GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS 0x90EB
-- #define GL_MAX_COMPUTE_WORK_GROUP_COUNT   0x91BE
-- #define GL_MAX_COMPUTE_WORK_GROUP_SIZE    0x91BF
-- #define GL_COMPUTE_WORK_GROUP_SIZE        0x8267
-- #define GL_UNIFORM_BLOCK_REFERENCED_BY_COMPUTE_SHADER 0x90EC
-- #define GL_ATOMIC_COUNTER_BUFFER_REFERENCED_BY_COMPUTE_SHADER 0x90ED
-- #define GL_DISPATCH_INDIRECT_BUFFER       0x90EE
-- #define GL_DISPATCH_INDIRECT_BUFFER_BINDING 0x90EF
-- #define GL_COMPUTE_SHADER_BIT             0x00000020
-- #define GL_DEBUG_OUTPUT_SYNCHRONOUS       0x8242
-- #define GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH 0x8243
-- #define GL_DEBUG_CALLBACK_FUNCTION        0x8244
-- #define GL_DEBUG_CALLBACK_USER_PARAM      0x8245
-- #define GL_DEBUG_SOURCE_API               0x8246
-- #define GL_DEBUG_SOURCE_WINDOW_SYSTEM     0x8247
-- #define GL_DEBUG_SOURCE_SHADER_COMPILER   0x8248
-- #define GL_DEBUG_SOURCE_THIRD_PARTY       0x8249
-- #define GL_DEBUG_SOURCE_APPLICATION       0x824A
-- #define GL_DEBUG_SOURCE_OTHER             0x824B
-- #define GL_DEBUG_TYPE_ERROR               0x824C
-- #define GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR 0x824D
-- #define GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR  0x824E
-- #define GL_DEBUG_TYPE_PORTABILITY         0x824F
-- #define GL_DEBUG_TYPE_PERFORMANCE         0x8250
-- #define GL_DEBUG_TYPE_OTHER               0x8251
-- #define GL_MAX_DEBUG_MESSAGE_LENGTH       0x9143
-- #define GL_MAX_DEBUG_LOGGED_MESSAGES      0x9144
-- #define GL_DEBUG_LOGGED_MESSAGES          0x9145
-- #define GL_DEBUG_SEVERITY_HIGH            0x9146
-- #define GL_DEBUG_SEVERITY_MEDIUM          0x9147
-- #define GL_DEBUG_SEVERITY_LOW             0x9148
-- #define GL_DEBUG_TYPE_MARKER              0x8268
-- #define GL_DEBUG_TYPE_PUSH_GROUP          0x8269
-- #define GL_DEBUG_TYPE_POP_GROUP           0x826A
-- #define GL_DEBUG_SEVERITY_NOTIFICATION    0x826B
-- #define GL_MAX_DEBUG_GROUP_STACK_DEPTH    0x826C
-- #define GL_DEBUG_GROUP_STACK_DEPTH        0x826D
-- #define GL_BUFFER                         0x82E0
-- #define GL_SHADER                         0x82E1
-- #define GL_PROGRAM                        0x82E2
-- #define GL_QUERY                          0x82E3
-- #define GL_PROGRAM_PIPELINE               0x82E4
-- #define GL_SAMPLER                        0x82E6
-- #define GL_MAX_LABEL_LENGTH               0x82E8
-- #define GL_DEBUG_OUTPUT                   0x92E0
-- #define GL_CONTEXT_FLAG_DEBUG_BIT         0x00000002
-- #define GL_MAX_UNIFORM_LOCATIONS          0x826E
-- #define GL_FRAMEBUFFER_DEFAULT_WIDTH      0x9310
-- #define GL_FRAMEBUFFER_DEFAULT_HEIGHT     0x9311
-- #define GL_FRAMEBUFFER_DEFAULT_LAYERS     0x9312
-- #define GL_FRAMEBUFFER_DEFAULT_SAMPLES    0x9313
-- #define GL_FRAMEBUFFER_DEFAULT_FIXED_SAMPLE_LOCATIONS 0x9314
-- #define GL_MAX_FRAMEBUFFER_WIDTH          0x9315
-- #define GL_MAX_FRAMEBUFFER_HEIGHT         0x9316
-- #define GL_MAX_FRAMEBUFFER_LAYERS         0x9317
-- #define GL_MAX_FRAMEBUFFER_SAMPLES        0x9318
-- #define GL_INTERNALFORMAT_SUPPORTED       0x826F
-- #define GL_INTERNALFORMAT_PREFERRED       0x8270
-- #define GL_INTERNALFORMAT_RED_SIZE        0x8271
-- #define GL_INTERNALFORMAT_GREEN_SIZE      0x8272
-- #define GL_INTERNALFORMAT_BLUE_SIZE       0x8273
-- #define GL_INTERNALFORMAT_ALPHA_SIZE      0x8274
-- #define GL_INTERNALFORMAT_DEPTH_SIZE      0x8275
-- #define GL_INTERNALFORMAT_STENCIL_SIZE    0x8276
-- #define GL_INTERNALFORMAT_SHARED_SIZE     0x8277
-- #define GL_INTERNALFORMAT_RED_TYPE        0x8278
-- #define GL_INTERNALFORMAT_GREEN_TYPE      0x8279
-- #define GL_INTERNALFORMAT_BLUE_TYPE       0x827A
-- #define GL_INTERNALFORMAT_ALPHA_TYPE      0x827B
-- #define GL_INTERNALFORMAT_DEPTH_TYPE      0x827C
-- #define GL_INTERNALFORMAT_STENCIL_TYPE    0x827D
-- #define GL_MAX_WIDTH                      0x827E
-- #define GL_MAX_HEIGHT                     0x827F
-- #define GL_MAX_DEPTH                      0x8280
-- #define GL_MAX_LAYERS                     0x8281
-- #define GL_MAX_COMBINED_DIMENSIONS        0x8282
-- #define GL_COLOR_COMPONENTS               0x8283
-- #define GL_DEPTH_COMPONENTS               0x8284
-- #define GL_STENCIL_COMPONENTS             0x8285
-- #define GL_COLOR_RENDERABLE               0x8286
-- #define GL_DEPTH_RENDERABLE               0x8287
-- #define GL_STENCIL_RENDERABLE             0x8288
-- #define GL_FRAMEBUFFER_RENDERABLE         0x8289
-- #define GL_FRAMEBUFFER_RENDERABLE_LAYERED 0x828A
-- #define GL_FRAMEBUFFER_BLEND              0x828B
-- #define GL_READ_PIXELS                    0x828C
-- #define GL_READ_PIXELS_FORMAT             0x828D
-- #define GL_READ_PIXELS_TYPE               0x828E
-- #define GL_TEXTURE_IMAGE_FORMAT           0x828F
-- #define GL_TEXTURE_IMAGE_TYPE             0x8290
-- #define GL_GET_TEXTURE_IMAGE_FORMAT       0x8291
-- #define GL_GET_TEXTURE_IMAGE_TYPE         0x8292
-- #define GL_MIPMAP                         0x8293
-- #define GL_MANUAL_GENERATE_MIPMAP         0x8294
-- #define GL_AUTO_GENERATE_MIPMAP           0x8295
-- #define GL_COLOR_ENCODING                 0x8296
-- #define GL_SRGB_READ                      0x8297
-- #define GL_SRGB_WRITE                     0x8298
-- #define GL_FILTER                         0x829A
-- #define GL_VERTEX_TEXTURE                 0x829B
-- #define GL_TESS_CONTROL_TEXTURE           0x829C
-- #define GL_TESS_EVALUATION_TEXTURE        0x829D
-- #define GL_GEOMETRY_TEXTURE               0x829E
-- #define GL_FRAGMENT_TEXTURE               0x829F
-- #define GL_COMPUTE_TEXTURE                0x82A0
-- #define GL_TEXTURE_SHADOW                 0x82A1
-- #define GL_TEXTURE_GATHER                 0x82A2
-- #define GL_TEXTURE_GATHER_SHADOW          0x82A3
-- #define GL_SHADER_IMAGE_LOAD              0x82A4
-- #define GL_SHADER_IMAGE_STORE             0x82A5
-- #define GL_SHADER_IMAGE_ATOMIC            0x82A6
-- #define GL_IMAGE_TEXEL_SIZE               0x82A7
-- #define GL_IMAGE_COMPATIBILITY_CLASS      0x82A8
-- #define GL_IMAGE_PIXEL_FORMAT             0x82A9
-- #define GL_IMAGE_PIXEL_TYPE               0x82AA
-- #define GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_TEST 0x82AC
-- #define GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_TEST 0x82AD
-- #define GL_SIMULTANEOUS_TEXTURE_AND_DEPTH_WRITE 0x82AE
-- #define GL_SIMULTANEOUS_TEXTURE_AND_STENCIL_WRITE 0x82AF
-- #define GL_TEXTURE_COMPRESSED_BLOCK_WIDTH 0x82B1
-- #define GL_TEXTURE_COMPRESSED_BLOCK_HEIGHT 0x82B2
-- #define GL_TEXTURE_COMPRESSED_BLOCK_SIZE  0x82B3
-- #define GL_CLEAR_BUFFER                   0x82B4
-- #define GL_TEXTURE_VIEW                   0x82B5
-- #define GL_VIEW_COMPATIBILITY_CLASS       0x82B6
-- #define GL_FULL_SUPPORT                   0x82B7
-- #define GL_CAVEAT_SUPPORT                 0x82B8
-- #define GL_IMAGE_CLASS_4_X_32             0x82B9
-- #define GL_IMAGE_CLASS_2_X_32             0x82BA
-- #define GL_IMAGE_CLASS_1_X_32             0x82BB
-- #define GL_IMAGE_CLASS_4_X_16             0x82BC
-- #define GL_IMAGE_CLASS_2_X_16             0x82BD
-- #define GL_IMAGE_CLASS_1_X_16             0x82BE
-- #define GL_IMAGE_CLASS_4_X_8              0x82BF
-- #define GL_IMAGE_CLASS_2_X_8              0x82C0
-- #define GL_IMAGE_CLASS_1_X_8              0x82C1
-- #define GL_IMAGE_CLASS_11_11_10           0x82C2
-- #define GL_IMAGE_CLASS_10_10_10_2         0x82C3
-- #define GL_VIEW_CLASS_128_BITS            0x82C4
-- #define GL_VIEW_CLASS_96_BITS             0x82C5
-- #define GL_VIEW_CLASS_64_BITS             0x82C6
-- #define GL_VIEW_CLASS_48_BITS             0x82C7
-- #define GL_VIEW_CLASS_32_BITS             0x82C8
-- #define GL_VIEW_CLASS_24_BITS             0x82C9
-- #define GL_VIEW_CLASS_16_BITS             0x82CA
-- #define GL_VIEW_CLASS_8_BITS              0x82CB
-- #define GL_VIEW_CLASS_S3TC_DXT1_RGB       0x82CC
-- #define GL_VIEW_CLASS_S3TC_DXT1_RGBA      0x82CD
-- #define GL_VIEW_CLASS_S3TC_DXT3_RGBA      0x82CE
-- #define GL_VIEW_CLASS_S3TC_DXT5_RGBA      0x82CF
-- #define GL_VIEW_CLASS_RGTC1_RED           0x82D0
-- #define GL_VIEW_CLASS_RGTC2_RG            0x82D1
-- #define GL_VIEW_CLASS_BPTC_UNORM          0x82D2
-- #define GL_VIEW_CLASS_BPTC_FLOAT          0x82D3
-- #define GL_UNIFORM                        0x92E1
-- #define GL_UNIFORM_BLOCK                  0x92E2
-- #define GL_PROGRAM_INPUT                  0x92E3
-- #define GL_PROGRAM_OUTPUT                 0x92E4
-- #define GL_BUFFER_VARIABLE                0x92E5
-- #define GL_SHADER_STORAGE_BLOCK           0x92E6
-- #define GL_VERTEX_SUBROUTINE              0x92E8
-- #define GL_TESS_CONTROL_SUBROUTINE        0x92E9
-- #define GL_TESS_EVALUATION_SUBROUTINE     0x92EA
-- #define GL_GEOMETRY_SUBROUTINE            0x92EB
-- #define GL_FRAGMENT_SUBROUTINE            0x92EC
-- #define GL_COMPUTE_SUBROUTINE             0x92ED
-- #define GL_VERTEX_SUBROUTINE_UNIFORM      0x92EE
-- #define GL_TESS_CONTROL_SUBROUTINE_UNIFORM 0x92EF
-- #define GL_TESS_EVALUATION_SUBROUTINE_UNIFORM 0x92F0
-- #define GL_GEOMETRY_SUBROUTINE_UNIFORM    0x92F1
-- #define GL_FRAGMENT_SUBROUTINE_UNIFORM    0x92F2
-- #define GL_COMPUTE_SUBROUTINE_UNIFORM     0x92F3
-- #define GL_TRANSFORM_FEEDBACK_VARYING     0x92F4
-- #define GL_ACTIVE_RESOURCES               0x92F5
-- #define GL_MAX_NAME_LENGTH                0x92F6
-- #define GL_MAX_NUM_ACTIVE_VARIABLES       0x92F7
-- #define GL_MAX_NUM_COMPATIBLE_SUBROUTINES 0x92F8
-- #define GL_NAME_LENGTH                    0x92F9
-- #define GL_TYPE                           0x92FA
-- #define GL_ARRAY_SIZE                     0x92FB
-- #define GL_OFFSET                         0x92FC
-- #define GL_BLOCK_INDEX                    0x92FD
-- #define GL_ARRAY_STRIDE                   0x92FE
-- #define GL_MATRIX_STRIDE                  0x92FF
-- #define GL_IS_ROW_MAJOR                   0x9300
-- #define GL_ATOMIC_COUNTER_BUFFER_INDEX    0x9301
-- #define GL_BUFFER_BINDING                 0x9302
-- #define GL_BUFFER_DATA_SIZE               0x9303
-- #define GL_NUM_ACTIVE_VARIABLES           0x9304
-- #define GL_ACTIVE_VARIABLES               0x9305
-- #define GL_REFERENCED_BY_VERTEX_SHADER    0x9306
-- #define GL_REFERENCED_BY_TESS_CONTROL_SHADER 0x9307
-- #define GL_REFERENCED_BY_TESS_EVALUATION_SHADER 0x9308
-- #define GL_REFERENCED_BY_GEOMETRY_SHADER  0x9309
-- #define GL_REFERENCED_BY_FRAGMENT_SHADER  0x930A
-- #define GL_REFERENCED_BY_COMPUTE_SHADER   0x930B
-- #define GL_TOP_LEVEL_ARRAY_SIZE           0x930C
-- #define GL_TOP_LEVEL_ARRAY_STRIDE         0x930D
-- #define GL_LOCATION                       0x930E
-- #define GL_LOCATION_INDEX                 0x930F
-- #define GL_IS_PER_PATCH                   0x92E7
-- #define GL_SHADER_STORAGE_BUFFER          0x90D2
-- #define GL_SHADER_STORAGE_BUFFER_BINDING  0x90D3
-- #define GL_SHADER_STORAGE_BUFFER_START    0x90D4
-- #define GL_SHADER_STORAGE_BUFFER_SIZE     0x90D5
-- #define GL_MAX_VERTEX_SHADER_STORAGE_BLOCKS 0x90D6
-- #define GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS 0x90D7
-- #define GL_MAX_TESS_CONTROL_SHADER_STORAGE_BLOCKS 0x90D8
-- #define GL_MAX_TESS_EVALUATION_SHADER_STORAGE_BLOCKS 0x90D9
-- #define GL_MAX_FRAGMENT_SHADER_STORAGE_BLOCKS 0x90DA
-- #define GL_MAX_COMPUTE_SHADER_STORAGE_BLOCKS 0x90DB
-- #define GL_MAX_COMBINED_SHADER_STORAGE_BLOCKS 0x90DC
-- #define GL_MAX_SHADER_STORAGE_BUFFER_BINDINGS 0x90DD
-- #define GL_MAX_SHADER_STORAGE_BLOCK_SIZE  0x90DE
-- #define GL_SHADER_STORAGE_BUFFER_OFFSET_ALIGNMENT 0x90DF
-- #define GL_SHADER_STORAGE_BARRIER_BIT     0x00002000
-- #define GL_MAX_COMBINED_SHADER_OUTPUT_RESOURCES 0x8F39
-- #define GL_DEPTH_STENCIL_TEXTURE_MODE     0x90EA
-- #define GL_TEXTURE_BUFFER_OFFSET          0x919D
-- #define GL_TEXTURE_BUFFER_SIZE            0x919E
-- #define GL_TEXTURE_BUFFER_OFFSET_ALIGNMENT 0x919F
-- #define GL_TEXTURE_VIEW_MIN_LEVEL         0x82DB
-- #define GL_TEXTURE_VIEW_NUM_LEVELS        0x82DC
-- #define GL_TEXTURE_VIEW_MIN_LAYER         0x82DD
-- #define GL_TEXTURE_VIEW_NUM_LAYERS        0x82DE
-- #define GL_TEXTURE_IMMUTABLE_LEVELS       0x82DF
-- #define GL_VERTEX_ATTRIB_BINDING          0x82D4
-- #define GL_VERTEX_ATTRIB_RELATIVE_OFFSET  0x82D5
-- #define GL_VERTEX_BINDING_DIVISOR         0x82D6
-- #define GL_VERTEX_BINDING_OFFSET          0x82D7
-- #define GL_VERTEX_BINDING_STRIDE          0x82D8
-- #define GL_MAX_VERTEX_ATTRIB_RELATIVE_OFFSET 0x82D9
-- #define GL_MAX_VERTEX_ATTRIB_BINDINGS     0x82DA
-- #define GL_VERTEX_BINDING_BUFFER          0x8F4F
GLAPI(glClearBufferData, GLenum -> GLenum -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glClearBufferSubData, GLenum -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glDispatchCompute, GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glDispatchComputeIndirect, GLintptr -> GL ())
GLAPI(glCopyImageSubData, GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GL ())
GLAPI(glFramebufferParameteri, GLenum -> GLenum -> GLint -> GL ())
GLAPI(glGetFramebufferParameteriv, GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetInternalformati64v, GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint64 -> GL ())
GLAPI(glInvalidateTexSubImage, GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GL ())
GLAPI(glInvalidateTexImage, GLuint -> GLint -> GL ())
GLAPI(glInvalidateBufferSubData, GLuint -> GLintptr -> GLsizeiptr -> GL ())
GLAPI(glInvalidateBufferData, GLuint -> GL ())
GLAPI(glInvalidateFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> GL ())
GLAPI(glInvalidateSubFramebuffer, GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glMultiDrawArraysIndirect, GLenum -> Ptr () -> GLsizei -> GLsizei -> GL ())
GLAPI(glMultiDrawElementsIndirect, GLenum -> GLenum -> Ptr () -> GLsizei -> GLsizei -> GL ())
GLAPI(glGetProgramInterfaceiv, GLuint -> GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetProgramResourceIndex, GLuint -> GLenum -> CString -> GL GLuint)
GLAPI(glGetProgramResourceName, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glGetProgramResourceiv, GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> GL ())
GLAPI(glGetProgramResourceLocation, GLuint -> GLenum -> CString -> GL GLint)
GLAPI(glGetProgramResourceLocationIndex, GLuint -> GLenum -> CString -> GL GLint)
GLAPI(glShaderStorageBlockBinding, GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glTexBufferRange, GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> GL ())
GLAPI(glTexStorage2DMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> GL ())
GLAPI(glTexStorage3DMultisample, GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> GL ())
GLAPI(glTextureView, GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glBindVertexBuffer, GLuint -> GLuint -> GLintptr -> GLsizei -> GL ())
GLAPI(glVertexAttribFormat, GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> GL ())
GLAPI(glVertexAttribIFormat, GLuint -> GLint -> GLenum -> GLuint -> GL ())
GLAPI(glVertexAttribLFormat, GLuint -> GLint -> GLenum -> GLuint -> GL ())
GLAPI(glVertexAttribBinding, GLuint -> GLuint -> GL ())
GLAPI(glVertexBindingDivisor, GLuint -> GLuint -> GL ())
GLAPI(glDebugMessageControl, GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> GL ())
GLAPI(glDebugMessageInsert, GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> CString -> GL ())
GLAPI(glDebugMessageCallback, FunPtr GLDebugProc -> Ptr () -> GL ())
GLAPI(glGetDebugMessageLog, GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> CString -> GL GLuint)
GLAPI(glPushDebugGroup, GLenum -> GLuint -> GLsizei -> CString -> GL ())
GLAPI(glPopDebugGroup, GL ())
GLAPI(glObjectLabel, GLenum -> GLuint -> GLsizei -> CString -> GL ())
GLAPI(glGetObjectLabel, GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> CString -> GL ())
GLAPI(glObjectPtrLabel, Ptr () -> GLsizei -> CString -> GL ())
GLAPI(glGetObjectPtrLabel, Ptr () -> GLsizei -> Ptr GLsizei -> CString -> GL ())
-- #endif /* GL_VERSION_4_3 */
-- #ifndef GL_VERSION_4_4
-- #define GL_VERSION_4_4 1
-- #define GL_MAX_VERTEX_ATTRIB_STRIDE       0x82E5
-- #define GL_PRIMITIVE_RESTART_FOR_PATCHES_SUPPORTED 0x8221
-- #define GL_TEXTURE_BUFFER_BINDING         0x8C2A
-- #define GL_MAP_PERSISTENT_BIT             0x0040
-- #define GL_MAP_COHERENT_BIT               0x0080
-- #define GL_DYNAMIC_STORAGE_BIT            0x0100
-- #define GL_CLIENT_STORAGE_BIT             0x0200
-- #define GL_CLIENT_MAPPED_BUFFER_BARRIER_BIT 0x00004000
-- #define GL_BUFFER_IMMUTABLE_STORAGE       0x821F
-- #define GL_BUFFER_STORAGE_FLAGS           0x8220
-- #define GL_CLEAR_TEXTURE                  0x9365
-- #define GL_LOCATION_COMPONENT             0x934A
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_INDEX 0x934B
-- #define GL_TRANSFORM_FEEDBACK_BUFFER_STRIDE 0x934C
-- #define GL_QUERY_BUFFER                   0x9192
-- #define GL_QUERY_BUFFER_BARRIER_BIT       0x00008000
-- #define GL_QUERY_BUFFER_BINDING           0x9193
-- #define GL_QUERY_RESULT_NO_WAIT           0x9194
-- #define GL_MIRROR_CLAMP_TO_EDGE           0x8743
GLAPI(glBufferStorage, GLenum -> GLsizeiptr -> Ptr () -> GLbitfield -> GL ())
GLAPI(glClearTexImage, GLuint -> GLint -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glClearTexSubImage, GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glBindBuffersBase, GLenum -> GLuint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glBindBuffersRange, GLenum -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizeiptr -> GL ())
GLAPI(glBindTextures, GLuint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glBindSamplers, GLuint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glBindImageTextures, GLuint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glBindVertexBuffers, GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> GL ())
-- #endif /* GL_VERSION_4_4 */
-- #ifndef GL_VERSION_4_5
-- #define GL_VERSION_4_5 1
-- #define GL_CONTEXT_LOST                   0x0507
-- #define GL_NEGATIVE_ONE_TO_ONE            0x935E
-- #define GL_ZERO_TO_ONE                    0x935F
-- #define GL_CLIP_ORIGIN                    0x935C
-- #define GL_CLIP_DEPTH_MODE                0x935D
-- #define GL_QUERY_WAIT_INVERTED            0x8E17
-- #define GL_QUERY_NO_WAIT_INVERTED         0x8E18
-- #define GL_QUERY_BY_REGION_WAIT_INVERTED  0x8E19
-- #define GL_QUERY_BY_REGION_NO_WAIT_INVERTED 0x8E1A
-- #define GL_MAX_CULL_DISTANCES             0x82F9
-- #define GL_MAX_COMBINED_CLIP_AND_CULL_DISTANCES 0x82FA
-- #define GL_TEXTURE_TARGET                 0x1006
-- #define GL_QUERY_TARGET                   0x82EA
-- #define GL_TEXTURE_BINDING                0x82EB
-- #define GL_GUILTY_CONTEXT_RESET           0x8253
-- #define GL_INNOCENT_CONTEXT_RESET         0x8254
-- #define GL_UNKNOWN_CONTEXT_RESET          0x8255
-- #define GL_RESET_NOTIFICATION_STRATEGY    0x8256
-- #define GL_LOSE_CONTEXT_ON_RESET          0x8252
-- #define GL_NO_RESET_NOTIFICATION          0x8261
-- #define GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT 0x00000004
-- #define GL_CONTEXT_RELEASE_BEHAVIOR       0x82FB
-- #define GL_CONTEXT_RELEASE_BEHAVIOR_FLUSH 0x82FC
GLAPI(glClipControl, GLenum -> GLenum -> GL ())
GLAPI(glCreateTransformFeedbacks, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glTransformFeedbackBufferBase, GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glTransformFeedbackBufferRange, GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> GL ())
GLAPI(glGetTransformFeedbackiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetTransformFeedbacki_v, GLuint -> GLenum -> GLuint -> Ptr GLint -> GL ())
GLAPI(glGetTransformFeedbacki64_v, GLuint -> GLenum -> GLuint -> Ptr GLint64 -> GL ())
GLAPI(glCreateBuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glNamedBufferStorage, GLuint -> GLsizei -> Ptr () -> GLbitfield -> GL ())
GLAPI(glNamedBufferData, GLuint -> GLsizei -> Ptr () -> GLenum -> GL ())
GLAPI(glNamedBufferSubData, GLuint -> GLintptr -> GLsizei -> Ptr () -> GL ())
GLAPI(glCopyNamedBufferSubData, GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizei -> GL ())
GLAPI(glClearNamedBufferData, GLuint -> GLenum -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glClearNamedBufferSubData, GLuint -> GLenum -> GLintptr -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glMapNamedBuffer, GLuint -> GLenum -> GL (Ptr ()))
GLAPI(glMapNamedBufferRange, GLuint -> GLintptr -> GLsizei -> GLbitfield -> GL (Ptr ()))
GLAPI(glUnmapNamedBuffer, GLuint -> GL GLboolean)
GLAPI(glFlushMappedNamedBufferRange, GLuint -> GLintptr -> GLsizei -> GL ())
GLAPI(glGetNamedBufferParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetNamedBufferParameteri64v, GLuint -> GLenum -> Ptr GLint64 -> GL ())
GLAPI(glGetNamedBufferPointerv, GLuint -> GLenum -> Ptr (Ptr ()) -> GL ())
GLAPI(glGetNamedBufferSubData, GLuint -> GLintptr -> GLsizei -> Ptr () -> GL ())
GLAPI(glCreateFramebuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glNamedFramebufferRenderbuffer, GLuint -> GLenum -> GLenum -> GLuint -> GL ())
GLAPI(glNamedFramebufferParameteri, GLuint -> GLenum -> GLint -> GL ())
GLAPI(glNamedFramebufferTexture, GLuint -> GLenum -> GLuint -> GLint -> GL ())
GLAPI(glNamedFramebufferTextureLayer, GLuint -> GLenum -> GLuint -> GLint -> GLint -> GL ())
GLAPI(glNamedFramebufferDrawBuffer, GLuint -> GLenum -> GL ())
GLAPI(glNamedFramebufferDrawBuffers, GLuint -> GLsizei -> Ptr GLenum -> GL ())
GLAPI(glNamedFramebufferReadBuffer, GLuint -> GLenum -> GL ())
GLAPI(glInvalidateNamedFramebufferData, GLuint -> GLsizei -> Ptr GLenum -> GL ())
GLAPI(glInvalidateNamedFramebufferSubData, GLuint -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glClearNamedFramebufferiv, GLuint -> GLenum -> GLint -> Ptr GLint -> GL ())
GLAPI(glClearNamedFramebufferuiv, GLuint -> GLenum -> GLint -> Ptr GLuint -> GL ())
GLAPI(glClearNamedFramebufferfv, GLuint -> GLenum -> GLint -> Ptr GLfloat -> GL ())
GLAPI(glClearNamedFramebufferfi, GLuint -> GLenum -> GLfloat -> GLint -> GL ())
GLAPI(glBlitNamedFramebuffer, GLuint -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> GL ())
GLAPI(glCheckNamedFramebufferStatus, GLuint -> GLenum -> GL GLenum)
GLAPI(glGetNamedFramebufferParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetNamedFramebufferAttachmentParameteriv, GLuint -> GLenum -> GLenum -> Ptr GLint -> GL ())
GLAPI(glCreateRenderbuffers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glNamedRenderbufferStorage, GLuint -> GLenum -> GLsizei -> GLsizei -> GL ())
GLAPI(glNamedRenderbufferStorageMultisample, GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GL ())
GLAPI(glGetNamedRenderbufferParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glCreateTextures, GLenum -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glTextureBuffer, GLuint -> GLenum -> GLuint -> GL ())
GLAPI(glTextureBufferRange, GLuint -> GLenum -> GLuint -> GLintptr -> GLsizei -> GL ())
GLAPI(glTextureStorage1D, GLuint -> GLsizei -> GLenum -> GLsizei -> GL ())
GLAPI(glTextureStorage2D, GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GL ())
GLAPI(glTextureStorage3D, GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GL ())
GLAPI(glTextureStorage2DMultisample, GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> GL ())
GLAPI(glTextureStorage3DMultisample, GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> GL ())
GLAPI(glTextureSubImage1D, GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glTextureSubImage2D, GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glTextureSubImage3D, GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr () -> GL ())
GLAPI(glCompressedTextureSubImage1D, GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glCompressedTextureSubImage2D, GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glCompressedTextureSubImage3D, GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glCopyTextureSubImage1D, GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GL ())
GLAPI(glCopyTextureSubImage2D, GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glCopyTextureSubImage3D, GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GL ())
GLAPI(glTextureParameterf, GLuint -> GLenum -> GLfloat -> GL ())
GLAPI(glTextureParameterfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glTextureParameteri, GLuint -> GLenum -> GLint -> GL ())
GLAPI(glTextureParameterIiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glTextureParameterIuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glTextureParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGenerateTextureMipmap, GLuint -> GL ())
GLAPI(glBindTextureUnit, GLuint -> GLuint -> GL ())
GLAPI(glGetTextureImage, GLuint -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetCompressedTextureImage, GLuint -> GLint -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetTextureLevelParameterfv, GLuint -> GLint -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glGetTextureLevelParameteriv, GLuint -> GLint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetTextureParameterfv, GLuint -> GLenum -> Ptr GLfloat -> GL ())
GLAPI(glGetTextureParameterIiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetTextureParameterIuiv, GLuint -> GLenum -> Ptr GLuint -> GL ())
GLAPI(glGetTextureParameteriv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glCreateVertexArrays, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glDisableVertexArrayAttrib, GLuint -> GLuint -> GL ())
GLAPI(glEnableVertexArrayAttrib, GLuint -> GLuint -> GL ())
GLAPI(glVertexArrayElementBuffer, GLuint -> GLuint -> GL ())
GLAPI(glVertexArrayVertexBuffer, GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> GL ())
GLAPI(glVertexArrayVertexBuffers, GLuint -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> GL ())
GLAPI(glVertexArrayAttribBinding, GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glVertexArrayAttribFormat, GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> GL ())
GLAPI(glVertexArrayAttribIFormat, GLuint -> GLuint -> GLint -> GLenum -> GLuint -> GL ())
GLAPI(glVertexArrayAttribLFormat, GLuint -> GLuint -> GLint -> GLenum -> GLuint -> GL ())
GLAPI(glVertexArrayBindingDivisor, GLuint -> GLuint -> GLuint -> GL ())
GLAPI(glGetVertexArrayiv, GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetVertexArrayIndexediv, GLuint -> GLuint -> GLenum -> Ptr GLint -> GL ())
GLAPI(glGetVertexArrayIndexed64iv, GLuint -> GLuint -> GLenum -> Ptr GLint64 -> GL ())
GLAPI(glCreateSamplers, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glCreateProgramPipelines, GLsizei -> Ptr GLuint -> GL ())
GLAPI(glCreateQueries, GLenum -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGetQueryBufferObjecti64v, GLuint -> GLuint -> GLenum -> GLintptr -> GL ())
GLAPI(glGetQueryBufferObjectiv, GLuint -> GLuint -> GLenum -> GLintptr -> GL ())
GLAPI(glGetQueryBufferObjectui64v, GLuint -> GLuint -> GLenum -> GLintptr -> GL ())
GLAPI(glGetQueryBufferObjectuiv, GLuint -> GLuint -> GLenum -> GLintptr -> GL ())
GLAPI(glMemoryBarrierByRegion, GLbitfield -> GL ())
GLAPI(glGetTextureSubImage, GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetCompressedTextureSubImage, GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetGraphicsResetStatus, GL GLenum)
GLAPI(glGetnCompressedTexImage, GLenum -> GLint -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetnTexImage, GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetnUniformdv, GLuint -> GLint -> GLsizei -> Ptr GLdouble -> GL ())
GLAPI(glGetnUniformfv, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glGetnUniformiv, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glGetnUniformuiv, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glReadnPixels, GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glTextureBarrier, GL ())
-- #endif /* GL_VERSION_4_5 */
-- #ifndef GL_ARB_ES2_compatibility
-- #define GL_ARB_ES2_compatibility 1
-- #endif /* GL_ARB_ES2_compatibility */
-- #ifndef GL_ARB_ES3_1_compatibility
-- #define GL_ARB_ES3_1_compatibility 1
-- #endif /* GL_ARB_ES3_1_compatibility */
-- #ifndef GL_ARB_ES3_compatibility
-- #define GL_ARB_ES3_compatibility 1
-- #endif /* GL_ARB_ES3_compatibility */
-- #ifndef GL_ARB_arrays_of_arrays
-- #define GL_ARB_arrays_of_arrays 1
-- #endif /* GL_ARB_arrays_of_arrays */
-- #ifndef GL_ARB_base_instance
-- #define GL_ARB_base_instance 1
-- #endif /* GL_ARB_base_instance */
-- #ifndef GL_ARB_bindless_texture
-- #define GL_ARB_bindless_texture 1
-- #define GL_UNSIGNED_INT64_ARB             0x140F
GLAPI(glGetTextureHandleARB, GLuint -> GL GLuint64)
GLAPI(glGetTextureSamplerHandleARB, GLuint -> GLuint -> GL GLuint64)
GLAPI(glMakeTextureHandleResidentARB, GLuint64 -> GL ())
GLAPI(glMakeTextureHandleNonResidentARB, GLuint64 -> GL ())
GLAPI(glGetImageHandleARB, GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GL GLuint64)
GLAPI(glMakeImageHandleResidentARB, GLuint64 -> GLenum -> GL ())
GLAPI(glMakeImageHandleNonResidentARB, GLuint64 -> GL ())
GLAPI(glUniformHandleui64ARB, GLint -> GLuint64 -> GL ())
GLAPI(glUniformHandleui64vARB, GLint -> GLsizei -> Ptr GLuint64 -> GL ())
GLAPI(glProgramUniformHandleui64ARB, GLuint -> GLint -> GLuint64 -> GL ())
GLAPI(glProgramUniformHandleui64vARB, GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> GL ())
GLAPI(glIsTextureHandleResidentARB, GLuint64 -> GL GLboolean)
GLAPI(glIsImageHandleResidentARB, GLuint64 -> GL GLboolean)
GLAPI(glVertexAttribL1ui64ARB, GLuint -> GLuint64EXT -> GL ())
GLAPI(glVertexAttribL1ui64vARB, GLuint -> Ptr GLuint64EXT -> GL ())
GLAPI(glGetVertexAttribLui64vARB, GLuint -> GLenum -> Ptr GLuint64EXT -> GL ())
-- #endif /* GL_ARB_bindless_texture */
-- #ifndef GL_ARB_blend_func_extended
-- #define GL_ARB_blend_func_extended 1
-- #endif /* GL_ARB_blend_func_extended */
-- #ifndef GL_ARB_buffer_storage
-- #define GL_ARB_buffer_storage 1
-- #endif /* GL_ARB_buffer_storage */
-- #ifndef GL_ARB_cl_event
-- #define GL_ARB_cl_event 1
-- #define GL_SYNC_CL_EVENT_ARB              0x8240
-- #define GL_SYNC_CL_EVENT_COMPLETE_ARB     0x8241
-- GLAPI(glCreateSyncFromCLeventARB, Ptr OpenCLContext -> Ptr OpenCLEvent -> GLbitfield -> GL GLsync)
-- #endif /* GL_ARB_cl_event */
-- #ifndef GL_ARB_clear_buffer_object
-- #define GL_ARB_clear_buffer_object 1
-- #endif /* GL_ARB_clear_buffer_object */
-- #ifndef GL_ARB_clear_texture
-- #define GL_ARB_clear_texture 1
-- #endif /* GL_ARB_clear_texture */
-- #ifndef GL_ARB_clip_control
-- #define GL_ARB_clip_control 1
-- #endif /* GL_ARB_clip_control */
-- #ifndef GL_ARB_compressed_texture_pixel_storage
-- #define GL_ARB_compressed_texture_pixel_storage 1
-- #endif /* GL_ARB_compressed_texture_pixel_storage */
-- #ifndef GL_ARB_compute_shader
-- #define GL_ARB_compute_shader 1
-- #endif /* GL_ARB_compute_shader */
-- #ifndef GL_ARB_compute_variable_group_size
-- #define GL_ARB_compute_variable_group_size 1
-- #define GL_MAX_COMPUTE_VARIABLE_GROUP_INVOCATIONS_ARB 0x9344
-- #define GL_MAX_COMPUTE_FIXED_GROUP_INVOCATIONS_ARB 0x90EB
-- #define GL_MAX_COMPUTE_VARIABLE_GROUP_SIZE_ARB 0x9345
-- #define GL_MAX_COMPUTE_FIXED_GROUP_SIZE_ARB 0x91BF
GLAPI(glDispatchComputeGroupSizeARB, GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GL ())
-- #endif /* GL_ARB_compute_variable_group_size */
-- #ifndef GL_ARB_conditional_render_inverted
-- #define GL_ARB_conditional_render_inverted 1
-- #endif /* GL_ARB_conditional_render_inverted */
-- #ifndef GL_ARB_conservative_depth
-- #define GL_ARB_conservative_depth 1
-- #endif /* GL_ARB_conservative_depth */
-- #ifndef GL_ARB_copy_buffer
-- #define GL_ARB_copy_buffer 1
-- #endif /* GL_ARB_copy_buffer */
-- #ifndef GL_ARB_copy_image
-- #define GL_ARB_copy_image 1
-- #endif /* GL_ARB_copy_image */
-- #ifndef GL_ARB_cull_distance
-- #define GL_ARB_cull_distance 1
-- #endif /* GL_ARB_cull_distance */
-- #ifndef GL_ARB_debug_output
-- #define GL_ARB_debug_output 1
-- #define GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB   0x8242
-- #define GL_DEBUG_NEXT_LOGGED_MESSAGE_LENGTH_ARB 0x8243
-- #define GL_DEBUG_CALLBACK_FUNCTION_ARB    0x8244
-- #define GL_DEBUG_CALLBACK_USER_PARAM_ARB  0x8245
-- #define GL_DEBUG_SOURCE_API_ARB           0x8246
-- #define GL_DEBUG_SOURCE_WINDOW_SYSTEM_ARB 0x8247
-- #define GL_DEBUG_SOURCE_SHADER_COMPILER_ARB 0x8248
-- #define GL_DEBUG_SOURCE_THIRD_PARTY_ARB   0x8249
-- #define GL_DEBUG_SOURCE_APPLICATION_ARB   0x824A
-- #define GL_DEBUG_SOURCE_OTHER_ARB         0x824B
-- #define GL_DEBUG_TYPE_ERROR_ARB           0x824C
-- #define GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB 0x824D
-- #define GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB 0x824E
-- #define GL_DEBUG_TYPE_PORTABILITY_ARB     0x824F
-- #define GL_DEBUG_TYPE_PERFORMANCE_ARB     0x8250
-- #define GL_DEBUG_TYPE_OTHER_ARB           0x8251
-- #define GL_MAX_DEBUG_MESSAGE_LENGTH_ARB   0x9143
-- #define GL_MAX_DEBUG_LOGGED_MESSAGES_ARB  0x9144
-- #define GL_DEBUG_LOGGED_MESSAGES_ARB      0x9145
-- #define GL_DEBUG_SEVERITY_HIGH_ARB        0x9146
-- #define GL_DEBUG_SEVERITY_MEDIUM_ARB      0x9147
-- #define GL_DEBUG_SEVERITY_LOW_ARB         0x9148
GLAPI(glDebugMessageControlARB, GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> GL ())
GLAPI(glDebugMessageInsertARB, GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> CString -> GL ())
GLAPI(glDebugMessageCallbackARB, FunPtr GLDebugProc -> Ptr () -> GL ())
GLAPI(glGetDebugMessageLogARB, GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> CString -> GL GLuint)
-- #endif /* GL_ARB_debug_output */
-- #ifndef GL_ARB_depth_buffer_float
-- #define GL_ARB_depth_buffer_float 1
-- #endif /* GL_ARB_depth_buffer_float */
-- #ifndef GL_ARB_depth_clamp
-- #define GL_ARB_depth_clamp 1
-- #endif /* GL_ARB_depth_clamp */
-- #ifndef GL_ARB_derivative_control
-- #define GL_ARB_derivative_control 1
-- #endif /* GL_ARB_derivative_control */
-- #ifndef GL_ARB_direct_state_access
-- #define GL_ARB_direct_state_access 1
-- #endif /* GL_ARB_direct_state_access */
-- #ifndef GL_ARB_draw_buffers_blend
-- #define GL_ARB_draw_buffers_blend 1
GLAPI(glBlendEquationiARB, GLuint -> GLenum -> GL ())
GLAPI(glBlendEquationSeparateiARB, GLuint -> GLenum -> GLenum -> GL ())
GLAPI(glBlendFunciARB, GLuint -> GLenum -> GLenum -> GL ())
GLAPI(glBlendFuncSeparateiARB, GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> GL ())
-- #endif /* GL_ARB_draw_buffers_blend */
-- #ifndef GL_ARB_draw_elements_base_vertex
-- #define GL_ARB_draw_elements_base_vertex 1
-- #endif /* GL_ARB_draw_elements_base_vertex */
-- #ifndef GL_ARB_draw_indirect
-- #define GL_ARB_draw_indirect 1
-- #endif /* GL_ARB_draw_indirect */
-- #ifndef GL_ARB_enhanced_layouts
-- #define GL_ARB_enhanced_layouts 1
-- #endif /* GL_ARB_enhanced_layouts */
-- #ifndef GL_ARB_explicit_attrib_location
-- #define GL_ARB_explicit_attrib_location 1
-- #endif /* GL_ARB_explicit_attrib_location */
-- #ifndef GL_ARB_explicit_uniform_location
-- #define GL_ARB_explicit_uniform_location 1
-- #endif /* GL_ARB_explicit_uniform_location */
-- #ifndef GL_ARB_fragment_coord_conventions
-- #define GL_ARB_fragment_coord_conventions 1
-- #endif /* GL_ARB_fragment_coord_conventions */
-- #ifndef GL_ARB_fragment_layer_viewport
-- #define GL_ARB_fragment_layer_viewport 1
-- #endif /* GL_ARB_fragment_layer_viewport */
-- #ifndef GL_ARB_framebuffer_no_attachments
-- #define GL_ARB_framebuffer_no_attachments 1
-- #endif /* GL_ARB_framebuffer_no_attachments */
-- #ifndef GL_ARB_framebuffer_object
-- #define GL_ARB_framebuffer_object 1
-- #endif /* GL_ARB_framebuffer_object */
-- #ifndef GL_ARB_framebuffer_sRGB
-- #define GL_ARB_framebuffer_sRGB 1
-- #endif /* GL_ARB_framebuffer_sRGB */
-- #ifndef GL_ARB_get_program_binary
-- #define GL_ARB_get_program_binary 1
-- #endif /* GL_ARB_get_program_binary */
-- #ifndef GL_ARB_get_texture_sub_image
-- #define GL_ARB_get_texture_sub_image 1
-- #endif /* GL_ARB_get_texture_sub_image */
-- #ifndef GL_ARB_gpu_shader5
-- #define GL_ARB_gpu_shader5 1
-- #endif /* GL_ARB_gpu_shader5 */
-- #ifndef GL_ARB_gpu_shader_fp64
-- #define GL_ARB_gpu_shader_fp64 1
-- #endif /* GL_ARB_gpu_shader_fp64 */
-- #ifndef GL_ARB_half_float_vertex
-- #define GL_ARB_half_float_vertex 1
-- #endif /* GL_ARB_half_float_vertex */
-- #ifndef GL_ARB_imaging
-- #define GL_ARB_imaging 1
-- #define GL_BLEND_COLOR                    0x8005
-- #define GL_BLEND_EQUATION                 0x8009
-- #endif /* GL_ARB_imaging */
-- #ifndef GL_ARB_indirect_parameters
-- #define GL_ARB_indirect_parameters 1
-- #define GL_PARAMETER_BUFFER_ARB           0x80EE
-- #define GL_PARAMETER_BUFFER_BINDING_ARB   0x80EF
GLAPI(glMultiDrawArraysIndirectCountARB, GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> GL ())
GLAPI(glMultiDrawElementsIndirectCountARB, GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> GL ())
-- #endif /* GL_ARB_indirect_parameters */
-- #ifndef GL_ARB_internalformat_query
-- #define GL_ARB_internalformat_query 1
-- #endif /* GL_ARB_internalformat_query */
-- #ifndef GL_ARB_internalformat_query2
-- #define GL_ARB_internalformat_query2 1
-- #define GL_SRGB_DECODE_ARB                0x8299
-- #endif /* GL_ARB_internalformat_query2 */
-- #ifndef GL_ARB_invalidate_subdata
-- #define GL_ARB_invalidate_subdata 1
-- #endif /* GL_ARB_invalidate_subdata */
-- #ifndef GL_ARB_map_buffer_alignment
-- #define GL_ARB_map_buffer_alignment 1
-- #endif /* GL_ARB_map_buffer_alignment */
-- #ifndef GL_ARB_map_buffer_range
-- #define GL_ARB_map_buffer_range 1
-- #endif /* GL_ARB_map_buffer_range */
-- #ifndef GL_ARB_multi_bind
-- #define GL_ARB_multi_bind 1
-- #endif /* GL_ARB_multi_bind */
-- #ifndef GL_ARB_multi_draw_indirect
-- #define GL_ARB_multi_draw_indirect 1
-- #endif /* GL_ARB_multi_draw_indirect */
-- #ifndef GL_ARB_occlusion_query2
-- #define GL_ARB_occlusion_query2 1
-- #endif /* GL_ARB_occlusion_query2 */
-- #ifndef GL_ARB_pipeline_statistics_query
-- #define GL_ARB_pipeline_statistics_query 1
-- #define GL_VERTICES_SUBMITTED_ARB         0x82EE
-- #define GL_PRIMITIVES_SUBMITTED_ARB       0x82EF
-- #define GL_VERTEX_SHADER_INVOCATIONS_ARB  0x82F0
-- #define GL_TESS_CONTROL_SHADER_PATCHES_ARB 0x82F1
-- #define GL_TESS_EVALUATION_SHADER_INVOCATIONS_ARB 0x82F2
-- #define GL_GEOMETRY_SHADER_PRIMITIVES_EMITTED_ARB 0x82F3
-- #define GL_FRAGMENT_SHADER_INVOCATIONS_ARB 0x82F4
-- #define GL_COMPUTE_SHADER_INVOCATIONS_ARB 0x82F5
-- #define GL_CLIPPING_INPUT_PRIMITIVES_ARB  0x82F6
-- #define GL_CLIPPING_OUTPUT_PRIMITIVES_ARB 0x82F7
-- #endif /* GL_ARB_pipeline_statistics_query */
-- #ifndef GL_ARB_program_interface_query
-- #define GL_ARB_program_interface_query 1
-- #endif /* GL_ARB_program_interface_query */
-- #ifndef GL_ARB_provoking_vertex
-- #define GL_ARB_provoking_vertex 1
-- #endif /* GL_ARB_provoking_vertex */
-- #ifndef GL_ARB_query_buffer_object
-- #define GL_ARB_query_buffer_object 1
-- #endif /* GL_ARB_query_buffer_object */
-- #ifndef GL_ARB_robust_buffer_access_behavior
-- #define GL_ARB_robust_buffer_access_behavior 1
-- #endif /* GL_ARB_robust_buffer_access_behavior */
-- #ifndef GL_ARB_robustness
-- #define GL_ARB_robustness 1
-- #define GL_CONTEXT_FLAG_ROBUST_ACCESS_BIT_ARB 0x00000004
-- #define GL_LOSE_CONTEXT_ON_RESET_ARB      0x8252
-- #define GL_GUILTY_CONTEXT_RESET_ARB       0x8253
-- #define GL_INNOCENT_CONTEXT_RESET_ARB     0x8254
-- #define GL_UNKNOWN_CONTEXT_RESET_ARB      0x8255
-- #define GL_RESET_NOTIFICATION_STRATEGY_ARB 0x8256
-- #define GL_NO_RESET_NOTIFICATION_ARB      0x8261
GLAPI(glGetGraphicsResetStatusARB, GL GLenum)
GLAPI(glGetnTexImageARB, GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glReadnPixelsARB, GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetnCompressedTexImageARB, GLenum -> GLint -> GLsizei -> Ptr () -> GL ())
GLAPI(glGetnUniformfvARB, GLuint -> GLint -> GLsizei -> Ptr GLfloat -> GL ())
GLAPI(glGetnUniformivARB, GLuint -> GLint -> GLsizei -> Ptr GLint -> GL ())
GLAPI(glGetnUniformuivARB, GLuint -> GLint -> GLsizei -> Ptr GLuint -> GL ())
GLAPI(glGetnUniformdvARB, GLuint -> GLint -> GLsizei -> Ptr GLdouble -> GL ())
-- #endif /* GL_ARB_robustness */
-- #ifndef GL_ARB_robustness_isolation
-- #define GL_ARB_robustness_isolation 1
-- #endif /* GL_ARB_robustness_isolation */
-- #ifndef GL_ARB_sample_shading
-- #define GL_ARB_sample_shading 1
-- #define GL_SAMPLE_SHADING_ARB             0x8C36
-- #define GL_MIN_SAMPLE_SHADING_VALUE_ARB   0x8C37
GLAPI(glMinSampleShadingARB, GLfloat -> GL ())
-- #endif /* GL_ARB_sample_shading */
-- #ifndef GL_ARB_sampler_objects
-- #define GL_ARB_sampler_objects 1
-- #endif /* GL_ARB_sampler_objects */
-- #ifndef GL_ARB_seamless_cube_map
-- #define GL_ARB_seamless_cube_map 1
-- #endif /* GL_ARB_seamless_cube_map */
-- #ifndef GL_ARB_seamless_cubemap_per_texture
-- #define GL_ARB_seamless_cubemap_per_texture 1
-- #endif /* GL_ARB_seamless_cubemap_per_texture */
-- #ifndef GL_ARB_separate_shader_objects
-- #define GL_ARB_separate_shader_objects 1
-- #endif /* GL_ARB_separate_shader_objects */
-- #ifndef GL_ARB_shader_atomic_counters
-- #define GL_ARB_shader_atomic_counters 1
-- #endif /* GL_ARB_shader_atomic_counters */
-- #ifndef GL_ARB_shader_bit_encoding
-- #define GL_ARB_shader_bit_encoding 1
-- #endif /* GL_ARB_shader_bit_encoding */
-- #ifndef GL_ARB_shader_draw_parameters
-- #define GL_ARB_shader_draw_parameters 1
-- #endif /* GL_ARB_shader_draw_parameters */
-- #ifndef GL_ARB_shader_group_vote
-- #define GL_ARB_shader_group_vote 1
-- #endif /* GL_ARB_shader_group_vote */
-- #ifndef GL_ARB_shader_image_load_store
-- #define GL_ARB_shader_image_load_store 1
-- #endif /* GL_ARB_shader_image_load_store */
-- #ifndef GL_ARB_shader_image_size
-- #define GL_ARB_shader_image_size 1
-- #endif /* GL_ARB_shader_image_size */
-- #ifndef GL_ARB_shader_precision
-- #define GL_ARB_shader_precision 1
-- #endif /* GL_ARB_shader_precision */
-- #ifndef GL_ARB_shader_stencil_export
-- #define GL_ARB_shader_stencil_export 1
-- #endif /* GL_ARB_shader_stencil_export */
-- #ifndef GL_ARB_shader_storage_buffer_object
-- #define GL_ARB_shader_storage_buffer_object 1
-- #endif /* GL_ARB_shader_storage_buffer_object */
-- #ifndef GL_ARB_shader_subroutine
-- #define GL_ARB_shader_subroutine 1
-- #endif /* GL_ARB_shader_subroutine */
-- #ifndef GL_ARB_shader_texture_image_samples
-- #define GL_ARB_shader_texture_image_samples 1
-- #endif /* GL_ARB_shader_texture_image_samples */
-- #ifndef GL_ARB_shading_language_420pack
-- #define GL_ARB_shading_language_420pack 1
-- #endif /* GL_ARB_shading_language_420pack */
-- #ifndef GL_ARB_shading_language_include
-- #define GL_ARB_shading_language_include 1
-- #define GL_SHADER_INCLUDE_ARB             0x8DAE
-- #define GL_NAMED_STRING_LENGTH_ARB        0x8DE9
-- #define GL_NAMED_STRING_TYPE_ARB          0x8DEA
GLAPI(glNamedStringARB, GLenum -> GLint -> CString -> GLint -> CString -> GL ())
GLAPI(glDeleteNamedStringARB, GLint -> CString -> GL ())
GLAPI(glCompileShaderIncludeARB, GLuint -> GLsizei -> Ptr CString -> Ptr GLint -> GL ())
GLAPI(glIsNamedStringARB, GLint -> CString -> GL GLboolean)
GLAPI(glGetNamedStringARB, GLint -> CString -> GLsizei -> Ptr GLint -> CString -> GL ())
GLAPI(glGetNamedStringivARB, GLint -> CString -> GLenum -> Ptr GLint -> GL ())
-- #endif /* GL_ARB_shading_language_include */
-- #ifndef GL_ARB_shading_language_packing
-- #define GL_ARB_shading_language_packing 1
-- #endif /* GL_ARB_shading_language_packing */
-- #ifndef GL_ARB_sparse_buffer
-- #define GL_ARB_sparse_buffer 1
-- #define GL_SPARSE_STORAGE_BIT_ARB         0x0400
-- #define GL_SPARSE_BUFFER_PAGE_SIZE_ARB    0x82F8
GLAPI(glBufferPageCommitmentARB, GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> GL ())
GLAPI(glNamedBufferPageCommitmentEXT, GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> GL ())
GLAPI(glNamedBufferPageCommitmentARB, GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> GL ())
-- #endif /* GL_ARB_sparse_buffer */
-- #ifndef GL_ARB_sparse_texture
-- #define GL_ARB_sparse_texture 1
-- #define GL_TEXTURE_SPARSE_ARB             0x91A6
-- #define GL_VIRTUAL_PAGE_SIZE_INDEX_ARB    0x91A7
-- #define GL_NUM_SPARSE_LEVELS_ARB          0x91AA
-- #define GL_NUM_VIRTUAL_PAGE_SIZES_ARB     0x91A8
-- #define GL_VIRTUAL_PAGE_SIZE_X_ARB        0x9195
-- #define GL_VIRTUAL_PAGE_SIZE_Y_ARB        0x9196
-- #define GL_VIRTUAL_PAGE_SIZE_Z_ARB        0x9197
-- #define GL_MAX_SPARSE_TEXTURE_SIZE_ARB    0x9198
-- #define GL_MAX_SPARSE_3D_TEXTURE_SIZE_ARB 0x9199
-- #define GL_MAX_SPARSE_ARRAY_TEXTURE_LAYERS_ARB 0x919A
-- #define GL_SPARSE_TEXTURE_FULL_ARRAY_CUBE_MIPMAPS_ARB 0x91A9
GLAPI(glTexPageCommitmentARB, GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> GL ())
-- #endif /* GL_ARB_sparse_texture */
-- #ifndef GL_ARB_stencil_texturing
-- #define GL_ARB_stencil_texturing 1
-- #endif /* GL_ARB_stencil_texturing */
-- #ifndef GL_ARB_sync
-- #define GL_ARB_sync 1
-- #endif /* GL_ARB_sync */
-- #ifndef GL_ARB_tessellation_shader
-- #define GL_ARB_tessellation_shader 1
-- #endif /* GL_ARB_tessellation_shader */
-- #ifndef GL_ARB_texture_barrier
-- #define GL_ARB_texture_barrier 1
-- #endif /* GL_ARB_texture_barrier */
-- #ifndef GL_ARB_texture_buffer_object_rgb32
-- #define GL_ARB_texture_buffer_object_rgb32 1
-- #endif /* GL_ARB_texture_buffer_object_rgb32 */
-- #ifndef GL_ARB_texture_buffer_range
-- #define GL_ARB_texture_buffer_range 1
-- #endif /* GL_ARB_texture_buffer_range */
-- #ifndef GL_ARB_texture_compression_bptc
-- #define GL_ARB_texture_compression_bptc 1
-- #define GL_COMPRESSED_RGBA_BPTC_UNORM_ARB 0x8E8C
-- #define GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM_ARB 0x8E8D
-- #define GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT_ARB 0x8E8E
-- #define GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_ARB 0x8E8F
-- #endif /* GL_ARB_texture_compression_bptc */
-- #ifndef GL_ARB_texture_compression_rgtc
-- #define GL_ARB_texture_compression_rgtc 1
-- #endif /* GL_ARB_texture_compression_rgtc */
-- #ifndef GL_ARB_texture_cube_map_array
-- #define GL_ARB_texture_cube_map_array 1
-- #define GL_TEXTURE_CUBE_MAP_ARRAY_ARB     0x9009
-- #define GL_TEXTURE_BINDING_CUBE_MAP_ARRAY_ARB 0x900A
-- #define GL_PROXY_TEXTURE_CUBE_MAP_ARRAY_ARB 0x900B
-- #define GL_SAMPLER_CUBE_MAP_ARRAY_ARB     0x900C
-- #define GL_SAMPLER_CUBE_MAP_ARRAY_SHADOW_ARB 0x900D
-- #define GL_INT_SAMPLER_CUBE_MAP_ARRAY_ARB 0x900E
-- #define GL_UNSIGNED_INT_SAMPLER_CUBE_MAP_ARRAY_ARB 0x900F
-- #endif /* GL_ARB_texture_cube_map_array */
-- #ifndef GL_ARB_texture_gather
-- #define GL_ARB_texture_gather 1
-- #define GL_MIN_PROGRAM_TEXTURE_GATHER_OFFSET_ARB 0x8E5E
-- #define GL_MAX_PROGRAM_TEXTURE_GATHER_OFFSET_ARB 0x8E5F
-- #define GL_MAX_PROGRAM_TEXTURE_GATHER_COMPONENTS_ARB 0x8F9F
-- #endif /* GL_ARB_texture_gather */
-- #ifndef GL_ARB_texture_mirror_clamp_to_edge
-- #define GL_ARB_texture_mirror_clamp_to_edge 1
-- #endif /* GL_ARB_texture_mirror_clamp_to_edge */
-- #ifndef GL_ARB_texture_multisample
-- #define GL_ARB_texture_multisample 1
-- #endif /* GL_ARB_texture_multisample */
-- #ifndef GL_ARB_texture_query_levels
-- #define GL_ARB_texture_query_levels 1
-- #endif /* GL_ARB_texture_query_levels */
-- #ifndef GL_ARB_texture_query_lod
-- #define GL_ARB_texture_query_lod 1
-- #endif /* GL_ARB_texture_query_lod */
-- #ifndef GL_ARB_texture_rg
-- #define GL_ARB_texture_rg 1
-- #endif /* GL_ARB_texture_rg */
-- #ifndef GL_ARB_texture_rgb10_a2ui
-- #define GL_ARB_texture_rgb10_a2ui 1
-- #endif /* GL_ARB_texture_rgb10_a2ui */
-- #ifndef GL_ARB_texture_stencil8
-- #define GL_ARB_texture_stencil8 1
-- #endif /* GL_ARB_texture_stencil8 */
-- #ifndef GL_ARB_texture_storage
-- #define GL_ARB_texture_storage 1
-- #endif /* GL_ARB_texture_storage */
-- #ifndef GL_ARB_texture_storage_multisample
-- #define GL_ARB_texture_storage_multisample 1
-- #endif /* GL_ARB_texture_storage_multisample */
-- #ifndef GL_ARB_texture_swizzle
-- #define GL_ARB_texture_swizzle 1
-- #endif /* GL_ARB_texture_swizzle */
-- #ifndef GL_ARB_texture_view
-- #define GL_ARB_texture_view 1
-- #endif /* GL_ARB_texture_view */
-- #ifndef GL_ARB_timer_query
-- #define GL_ARB_timer_query 1
-- #endif /* GL_ARB_timer_query */
-- #ifndef GL_ARB_transform_feedback2
-- #define GL_ARB_transform_feedback2 1
-- #endif /* GL_ARB_transform_feedback2 */
-- #ifndef GL_ARB_transform_feedback3
-- #define GL_ARB_transform_feedback3 1
-- #endif /* GL_ARB_transform_feedback3 */
-- #ifndef GL_ARB_transform_feedback_instanced
-- #define GL_ARB_transform_feedback_instanced 1
-- #endif /* GL_ARB_transform_feedback_instanced */
-- #ifndef GL_ARB_transform_feedback_overflow_query
-- #define GL_ARB_transform_feedback_overflow_query 1
-- #define GL_TRANSFORM_FEEDBACK_OVERFLOW_ARB 0x82EC
-- #define GL_TRANSFORM_FEEDBACK_STREAM_OVERFLOW_ARB 0x82ED
-- #endif /* GL_ARB_transform_feedback_overflow_query */
-- #ifndef GL_ARB_uniform_buffer_object
-- #define GL_ARB_uniform_buffer_object 1
-- #endif /* GL_ARB_uniform_buffer_object */
-- #ifndef GL_ARB_vertex_array_bgra
-- #define GL_ARB_vertex_array_bgra 1
-- #endif /* GL_ARB_vertex_array_bgra */
-- #ifndef GL_ARB_vertex_array_object
-- #define GL_ARB_vertex_array_object 1
-- #endif /* GL_ARB_vertex_array_object */
-- #ifndef GL_ARB_vertex_attrib_64bit
-- #define GL_ARB_vertex_attrib_64bit 1
-- #endif /* GL_ARB_vertex_attrib_64bit */
-- #ifndef GL_ARB_vertex_attrib_binding
-- #define GL_ARB_vertex_attrib_binding 1
-- #endif /* GL_ARB_vertex_attrib_binding */
-- #ifndef GL_ARB_vertex_type_10f_11f_11f_rev
-- #define GL_ARB_vertex_type_10f_11f_11f_rev 1
-- #endif /* GL_ARB_vertex_type_10f_11f_11f_rev */
-- #ifndef GL_ARB_vertex_type_2_10_10_10_rev
-- #define GL_ARB_vertex_type_2_10_10_10_rev 1
-- #endif /* GL_ARB_vertex_type_2_10_10_10_rev */
-- #ifndef GL_ARB_viewport_array
-- #define GL_ARB_viewport_array 1
-- #endif /* GL_ARB_viewport_array */
-- #ifndef GL_KHR_context_flush_control
-- #define GL_KHR_context_flush_control 1
-- #endif /* GL_KHR_context_flush_control */
-- #ifndef GL_KHR_debug
-- #define GL_KHR_debug 1
-- #endif /* GL_KHR_debug */
-- #ifndef GL_KHR_robust_buffer_access_behavior
-- #define GL_KHR_robust_buffer_access_behavior 1
-- #endif /* GL_KHR_robust_buffer_access_behavior */
-- #ifndef GL_KHR_robustness
-- #define GL_KHR_robustness 1
-- #define GL_CONTEXT_ROBUST_ACCESS          0x90F3
-- #endif /* GL_KHR_robustness */
-- #ifndef GL_KHR_texture_compression_astc_hdr
-- #define GL_KHR_texture_compression_astc_hdr 1
-- #define GL_COMPRESSED_RGBA_ASTC_4x4_KHR   0x93B0
-- #define GL_COMPRESSED_RGBA_ASTC_5x4_KHR   0x93B1
-- #define GL_COMPRESSED_RGBA_ASTC_5x5_KHR   0x93B2
-- #define GL_COMPRESSED_RGBA_ASTC_6x5_KHR   0x93B3
-- #define GL_COMPRESSED_RGBA_ASTC_6x6_KHR   0x93B4
-- #define GL_COMPRESSED_RGBA_ASTC_8x5_KHR   0x93B5
-- #define GL_COMPRESSED_RGBA_ASTC_8x6_KHR   0x93B6
-- #define GL_COMPRESSED_RGBA_ASTC_8x8_KHR   0x93B7
-- #define GL_COMPRESSED_RGBA_ASTC_10x5_KHR  0x93B8
-- #define GL_COMPRESSED_RGBA_ASTC_10x6_KHR  0x93B9
-- #define GL_COMPRESSED_RGBA_ASTC_10x8_KHR  0x93BA
-- #define GL_COMPRESSED_RGBA_ASTC_10x10_KHR 0x93BB
-- #define GL_COMPRESSED_RGBA_ASTC_12x10_KHR 0x93BC
-- #define GL_COMPRESSED_RGBA_ASTC_12x12_KHR 0x93BD
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR 0x93D0
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR 0x93D1
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR 0x93D2
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR 0x93D3
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR 0x93D4
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR 0x93D5
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR 0x93D6
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR 0x93D7
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR 0x93D8
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR 0x93D9
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR 0x93DA
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR 0x93DB
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR 0x93DC
-- #define GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR 0x93DD
-- #endif /* GL_KHR_texture_compression_astc_hdr */
-- #ifndef GL_KHR_texture_compression_astc_ldr
-- #define GL_KHR_texture_compression_astc_ldr 1
-- #endif /* GL_KHR_texture_compression_astc_ldr */
