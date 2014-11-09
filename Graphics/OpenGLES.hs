-- | OpenGL ES (ES for Embed Systems) 2.0 and 3.0
-- 
-- - <https://www.khronos.org/registry/gles/specs/3.1/es_spec_3.1.withchanges.pdf OpenGL® ES Version 3.1 (June 4, 2014) [PDF]>
-- - <http://www.khronos.org/opengles/sdk/docs/man31/ OpenGL ES 3.1 Manual>
-- - <http://www.khronos.org/files/opengles3-quick-reference-card.pdf OpenGL ES 3.0 Quick Reference [PDF]>
-- - <http://www.khronos.org/opengles/sdk/docs/man3/ OpenGL ES 3.0 Manual>
-- - <http://www.khronos.org/opengles/sdk/docs/reference_cards/OpenGL-ES-2_0-Reference-card.pdf OpenGL ES 2.0 Quick Reference [PDF]>
-- - <http://www.khronos.org/opengles/sdk/docs/man/ OpenGL ES 2.0 Manual>
-- - <http://code.google.com/p/angleproject/ ANGLE>
-- OpenGL ES backend for Windows.

module Graphics.OpenGLES (
  module Control.Future,
  module Data.Int,
  module Data.Word,
  module Graphics.OpenGLES.Buffer,
  module Graphics.OpenGLES.Framebuffer,
  module Graphics.OpenGLES.Caps,
  module Graphics.OpenGLES.Core,
  module Graphics.OpenGLES.PixelFormat,
  module Graphics.OpenGLES.State,
  module Graphics.OpenGLES.Texture,
  module Graphics.OpenGLES.Types,
  module Linear
  ) where
import Control.Future
import Data.Int
import Data.Word
import Graphics.OpenGLES.Buffer
import Graphics.OpenGLES.Framebuffer
import Graphics.OpenGLES.Caps
import Graphics.OpenGLES.Core
import Graphics.OpenGLES.PixelFormat
import Graphics.OpenGLES.State
import Graphics.OpenGLES.Texture
import Graphics.OpenGLES.Types
import Linear
