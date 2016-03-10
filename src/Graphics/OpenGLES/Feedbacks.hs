module Graphics.OpenGLES.Feedbacks where

-- | /ES 3.0+/
-- BeginQuery :: QueryType -> GLCall GLError
-- EndQuery :: GLCall ()
data QueryTarget = AnySamplesPassed | AnySamplesPassedConservative
instance BindTarget	QueryId QueryTarget
instance Marshal QueryTarget where
	marshal AnySamplesPassed = 0x8C2F
	marshal AnySamplesPassedConservative = 0x8D6A
--TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN

-- | /ES 3.0+/
data TransformFeedbackTarget = TransformFeedback
instance BindTarget	TransformFeedback TransformFeedbackTarget
instance Marshal TransformFeedbackTarget where
	marshal TransformFeedback = 0x8E22

BeginTransformFeedback :: TransformFeedback -> DrawMode -> GLCall ()
EndTransformFeedback :: GLCall ()

--type GLName = CString -- eliminate hungry [Char]s. +rewrite rule.
-- BindEGLContext,UnbindEGL

--useProgram :: ProgramId -> IO ()
--useProgram (ProgramId p) = glUseProgram p

--deleteProgram :: ProgramId -> IO ()
--deleteProgram (ProgramId p) = glDeleteProgram p

--DrawTextureExt -- XXX test: may not work with 2.0+ context.
--	:: !TextureObj
--	-> !Int32
--	-> !Int32
--	-> !Int32
--	-> !Int32
--	-> !Int32
--	-> !Int32
--	-> !Int32
--	-> !Int32
--	-> !Int32
--	-> GLCall GLError
