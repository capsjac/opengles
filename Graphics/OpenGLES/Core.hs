-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OpenGLES.Core
-- Copyright   :  (c) capsjac 2014
-- License     :  LGPL-3 (see the file LICENSE)
-- 
-- The neat and easy to use wrapper for OpenGL EmbedSystems (ES).
-- The wrapper is optimised for mobile and have small footprint.
-- Assuming OpenGL ES 2.0 or any later version, however, also works
-- with OpenGL 4.1/4.3+ on desktop.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Graphics.OpenGLES.Core (
  GL,
  -- * Lifecycle
  forkGL, stopGL, --destroyGL,
  endFrameGL, runGL, --runGLRes
  withGL, resetDrawQueue,
  glLog, glReadLogs, glLogContents,
  flushCommandQ, finishCommands,
  glFrameCount, glFlipping, framesize,
  
  -- * Draw Operation
  -- ** Clear Screen
  -- | See "Graphics.OpenGLES.Framebuffer"
  
  -- ** Draw
  glDraw,
  
  -- ** Draw Mode
  DrawMode, drawPoints, drawLines,
  drawLineLoop, drawLineStrip,
  drawTriangles, triangleStrip, triangleFan,
  
  -- ** Graphics State
  RenderConfig, renderTo,
  
  -- ** Programmable Shader
  Shader, vertexShader, fragmentShader, pixelShader,
  computeShader, geometryShader,
  tessellationEvalS, tessellationCtrlS,
  Program, module Data.Typeable,
  TransformFeedback(..), ProgramBinary,
  glCompile, glValidate,
  
  -- ** Uniform Variable
  Uniform, uniform, ($=), UnifVal, UniformAssignment,
  
  -- ** Vertex Attribute Array
  Attrib, attrib, normalized, divisor, (&=),
  VertexArray, glVA,
  VertexAttribute, AttrStruct, SetVertexAttr,
  
  -- ** Constant Vertex Attribute
  constAttrib,
  
  -- ** Texture
  -- | See "Graphics.OpenGLES.Texture"
  
  -- ** Vertex Picker
  VertexPicker, takeFrom,
  takeFromInstanced, takeFromMany,
  VertexIx, byIndex, byIndexInstanced,
  byIndices, byIndexLimited,
  drawCallSequence
  ) where
import Control.Applicative
import Control.Monad
import Control.Concurrent (forkOS, ThreadId, myThreadId, killThread)
import Control.Concurrent.Chan
import Control.Exception (catch, SomeException)
import Control.Future
import qualified Data.ByteString as B
import Data.IORef
import Data.Typeable
import Foreign
import Foreign.C.String (peekCStringLen)
import Graphics.OpenGLES.Base
import Graphics.OpenGLES.Buffer
import Graphics.OpenGLES.Env
import Graphics.OpenGLES.Internal


-- * Initialization

forkGL
	:: IO Bool
	-> GL ()
	-> GL ()
	-> IO ThreadId
forkGL resumeGL suspendGL swapBuffers = forkOS $ do
	writeIORef drawOrExit (Just swapBuffers)
	resumeGL -- Note: implicit glFlush here
	putStrLn "bindEGL"
	-- glRestoreLostObjects
	let loop count = do
		putStrLn $ "start draw " ++ show count
		readChan drawQueue >>= id
		loop (count + 1 :: Integer)
	catch (loop 0) $ \(e :: SomeException) -> do
		glLog $ "Rendering thread terminated: " ++ show e
		suspendGL
		putStrLn "unbindEGL"
		writeIORef drawOrExit (Just (glLog "Fatal lifecycle bug"))

stopGL :: IO ()
stopGL = do
	putStrLn "stopGL"
	writeIORef drawOrExit Nothing
	let waitGLThread = readIORef drawOrExit >>= \case
		Just _ -> nop
		Nothing -> waitGLThread
	waitGLThread
	putStrLn "Rendering has stopped."

--destroyGL :: IO ()
--destroyGL = runGL $ eglMakeCurrent Nothing, eglDestroyXXX ...

endFrameGL :: IO ()
endFrameGL = withGL go >>= waitFuture >> nop
	where go = do
		readIORef drawOrExit >>= \case
			Just eglSwapBuffer -> do
				eglSwapBuffer
				modifyIORef frameCounter (+1)
			Nothing -> myThreadId >>= killThread

runGL :: GL () -> IO ()
runGL io = writeChan drawQueue io

--runGLRes :: GL () -> IO ()
--runGLRes io = forkOS

withGL :: GL a -> IO (Future' a)
withGL io = asyncIO $ \update -> runGL (io >>= update . Finished)

-- | drawQueue may have drawcalls that use previous context,
-- so make it sure they are removed from the queue.
resetDrawQueue :: IO ()
resetDrawQueue = do
	isEmpty <- isEmptyChan drawQueue
	when (not isEmpty) (readChan drawQueue >> resetDrawQueue)

glReadLogs :: IO [String]
glReadLogs = do
	isEmpty <- isEmptyChan errorQueue
	if isEmpty
		then return []
		else (:) <$> readChan errorQueue <*> glReadLogs

glLogContents :: IO [String]
glLogContents = getChanContents errorQueue

flushCommandQ :: IO ()
flushCommandQ = runGL glFlush

finishCommands :: IO ()
finishCommands = runGL glFinish

-- | @return ()@
nop :: Monad m => m ()
nop = return ()

glFrameCount :: IO Int64
glFrameCount = readIORef frameCounter

glFlipping :: IO Bool
glFlipping = fmap odd glFrameCount

-- XXX bindFb defaultFramebuffer needed
-- | > GLFW.setFramebufferSizeCallback win $ Just (const framesize)
framesize :: Int -> Int -> IO ()
framesize w h = runGL $ glViewport 0 0 (f w) (f h)
	where f = fromIntegral


-- * Drawing

glDraw :: Typeable p
	=> DrawMode
	-> Program p
	-> [RenderConfig]
	-> [UniformAssignment p]
	-> VertexArray p
	-> VertexPicker
	-> GL Bool
glDraw (DrawMode mode) prog@(Program pobj _ _ _) setState unifs
		(VertexArray (vao, setVA)) (VertexPicker picker) = do
	glUseProgram =<< getObjId pobj
	sequence setState
	sequence unifs
	case extVAO of
		Nothing -> setVA
		Just (_, bind, _) -> getObjId vao >>= bind
	--glValidate prog
	picker mode

-- |
-- > renderTo $ do
-- >     bindFb defaultFramebuffer
-- >     viewport $ V4 0 0 512 512
-- >     depthRange $ V2 0.1 10.0
-- >     begin culling
-- >     cullFace hideBack
renderTo :: RenderConfig -> GL ()
renderTo = id


-- ** Draw Mode

drawPoints, drawLines, drawLineLoop, drawLineStrip,
	drawTriangles, triangleStrip, triangleFan :: DrawMode
drawPoints = DrawMode 0
drawLines = DrawMode 1
drawLineLoop = DrawMode 2
drawLineStrip = DrawMode 3
drawTriangles = DrawMode 4
triangleStrip = DrawMode 5
triangleFan = DrawMode 6


-- ** Programmable Shader

vertexShader, fragmentShader, pixelShader,
	computeShader, geometryShader,
	tessellationEvalS, tessellationCtrlS
	:: GLName -> B.ByteString -> Shader
vertexShader = Shader 0x8B31
fragmentShader = Shader 0x8B30
-- | Same as 'fragmentShader'
pixelShader = fragmentShader
-- | Compute shader requires /ES3.1+/
computeShader = Shader 0x91B9
-- | Geometry shader requires /GL_EXT_geometry_shader (ES3.1)/
geometryShader = Shader 0x8DD9
-- | Tessellation Shader requires /GL_EXT_tessellation_shader (ES3.1)/
tessellationEvalS = Shader 0x8E87
-- | Tessellation Shader requires /GL_EXT_tessellation_shader (ES3.1)/
tessellationCtrlS = Shader 0x8E88


glCompile
	:: Typeable p
	=> TransformFeedback
	-> [Shader]
	-> (Program p -> Int -> String -> Maybe ProgramBinary -> GL ())
	-> GL (Progress [String] (Program p))
glCompile tf shaders progressLogger = do
	glo <- newIORef undefined
	let prog = Program glo tf shaders ([],[])
	loadProgram prog (progressLogger prog)

-- | glValidateProgram checks to see whether the executables contained in
-- program can execute given the current OpenGL state.
glValidate :: Program p -> GL String
glValidate prog = alloca $ \intptr -> do
	pid <- getObjId $ programGLO prog
	glValidateProgram pid
	glGetProgramiv pid c_info_log_length intptr
	len <- fmap fromIntegral $ peek intptr
	info <- allocaBytes len $ \buf -> do
		glGetProgramInfoLog pid (fromIntegral len) nullPtr buf
		peekCStringLen (buf, len-1)
	glLog $ "validateProgram: " ++ info
	return info


-- ** Uniform Variable

type UniformAssignment p = GL ()

uniform
	:: forall p a. (UnifVal a, Typeable p)
	=> GLName -> IO (Uniform p a)
uniform name = do
	desc <- lookupVarDesc typ
	case desc of
		Nothing -> return $ Uniform (-1, 0, nullPtr)
		Just (unifs, _) ->
			case lookup name unifs of
				Just unif -> validateType unif
				Nothing -> glLog errmsg >> return (Uniform (-1, 0, nullPtr))
	where
		typ = typeRep (undefined :: Program p)
		errmsg = "Uniform not found: " ++ name ++ " (" ++ show typ ++ ")"
		validateType (loc, size, gltyp) = do
			-- Prevent drawtime allocation (it leaks, though)
			ptr <- mallocArray (fromIntegral size) :: IO (Ptr Float)
			return $ Uniform (loc, size, castPtr ptr)

infix 0 $=
($=) :: UnifVal a => Uniform p a -> a -> UniformAssignment p
Uniform desc $= value = glUniform desc value


-- ** Vertex Attribute

-- | @normalized color `divisor` 1 &= buffer@
attrib
	:: forall p a. (VertexAttribute a, Typeable p)
	=> GLName -> IO (Attrib p a)
attrib name = do
	desc <- lookupVarDesc typ
	case desc of
		Nothing -> return $ Attrib (-1, 0, 0, 0)
		Just (_, attrs) ->
			case lookup name attrs of
				Just attr -> validateType attr
				Nothing -> glLog errmsg >> return (Attrib (-1, 0, 0, 0))
	where
		typ = typeRep (undefined :: Program p)
		errmsg = "Attribute not found: " ++ name ++ " (" ++ show typ ++ ")"
		validateType (loc, size, gltyp) =
			return $ Attrib (fromIntegral loc, size, 0, 0)

normalized :: Attrib p a -> Attrib p a
normalized (Attrib (i, s, 0, d)) = Attrib (i, s, 1, d)
normalized _ = error "inapplicable use of 'normalized'"

divisor :: Attrib p a -> Word32 -> Attrib p a
divisor (Attrib (i, s, n, _)) d = Attrib (i, s, n, d)

type SetVertexAttr p = GL ()

infix 0 &=
(&=) :: AttrStruct a p b => a -> Buffer b -> SetVertexAttr p
attribs &= buf = do
	bindBuffer array_buffer buf
	glVertexBuffer attribs buf


glVA :: [SetVertexAttr p] -> GL (VertexArray p)
glVA attrs = do
	let setVA = sequence_ attrs
	glo <- case extVAO of
		Nothing -> return (error "GLO not used")
		Just (gen, bind, del) ->
			newGLO gen del (\i -> bind i >> setVA)
	return $ VertexArray (glo, setVA)


-- ** Constant Vertex Attribute

constAttrib :: VertexAttribute a => Attrib p a -> a -> SetVertexAttr p
constAttrib (Attrib (ix, _, _, _)) val = do
	glDisableVertexAttribArray ix
	glVertexAttrib ix val


-- ** Vertex Picker

-- Wrapping glDrawArrays
takeFrom :: Int32 -> Int32 -> VertexPicker
takeFrom first count =
	VertexPicker $ \mode -> do
		glDrawArrays mode first count
		showError "glDrawArrays"

-- Wrapping glDrawArraysInstanced[EXT]
takeFromInstanced :: Int32 -> Int32 -> Int32 -> VertexPicker
takeFromInstanced first count numInstances =
	VertexPicker $ \mode -> do
		glDrawArraysInstanced mode first count numInstances
		showError "glDrawArraysInstanced"

-- Wrapping glMultiDrawArraysEXT
takeFromMany :: [(Int32, Int32)] -> VertexPicker
takeFromMany list =
	VertexPicker $ \mode -> do
		forM_ list $ \(first, count) -> do
			glDrawArrays mode first count
			showError "glDrawArrays[]"
		--	showError "glMultiDrawElementsEXT"
		return True
-- TakeFromManyRaw (Buffer Int32) (Buffer Word32)

sizePtr :: Int32 -> Ptr ()
sizePtr = intPtrToPtr . fromIntegral

-- Wrapping glDrawElements
byIndex :: VertexIx a => Buffer a -> Int32 -> Int32 -> VertexPicker
byIndex buf first count =
	let (typ, stride) = vxix buf in
	VertexPicker $ \mode -> do
		bindBuffer element_array_buffer buf
		glDrawElements mode count typ (sizePtr $ first * stride)
		showError "glDrawElements"

-- Wrapping glDrawElementsInstanced[EXT]
byIndexInstanced :: VertexIx a => Buffer a -> Int32 -> Int32 -> Int32 -> VertexPicker
byIndexInstanced buf first count instances =
	let (typ, stride) = vxix buf in
	VertexPicker $ \mode -> do
		bindBuffer element_array_buffer buf
		glDrawElementsInstanced mode count typ
			(sizePtr $ first * stride) instances
		showError "glDrawElementsInstanced"

-- Wrapping glMultiDrawElementsEXT
byIndices :: VertexIx a => Buffer a -> [(Int32, Int32)] -> VertexPicker
byIndices buf list =
	let (typ, stride) = vxix buf in
	VertexPicker $ \mode -> do
		bindBuffer element_array_buffer buf
		forM_ list $ \(first, count) -> do
			glDrawElements mode count typ (sizePtr $ first * stride)
			showError "glDrawElements[]"
		return True
		--withFirstCountArray list $ \cptr iptr clen -> do
		--	glMultiDrawElementsEXT mode cptr typ iptr (clen * stride)
		--	showError "glMultiDrawElementsEXT"
-- ByIndicesRaw (Buffer w) (Buffer Word32) (Buffer Word32)

-- Wrapping glDrawRangeElements[EXT]
byIndexLimited :: VertexIx a => Buffer a -> Int32 -> Int32 -> Word32 -> Word32 -> VertexPicker
byIndexLimited buf first count min max =
	let (typ, stride) = vxix buf in
	VertexPicker $ \mode -> do
		bindBuffer element_array_buffer buf
		glDrawElements mode count typ (sizePtr $ first * stride)
		showError "glDrawElements'"
		--showError "glDrawRangeElements[EXT]"
-- FromToIndexRaw !BufferRef !Int !Int !GLsizei !GLenum !Int

drawCallSequence :: [VertexPicker] -> VertexPicker
drawCallSequence xs =
	VertexPicker $ \mode ->
		mapM_ (\(VertexPicker f) -> f mode) xs >> return True


