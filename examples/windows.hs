-- | > ghc -threaded windows.hs && windows.exe
-- Plain red screen will appear.
module Main　where

import System.Win32.DLL (getModuleHandle)
import Graphics.Win32
import Graphics.Win32.Message
import Graphics.Win32.Window
import Data.Int
import Data.Maybe
import Control.Monad
import Foreign.C.String

import Graphics.EGL
import Graphics.OpenGLES
import Graphics.OpenGLES.Base
import Foreign
import Foreign.C

foreign import stdcall "PostQuitMessage" postQuitMessage
   :: Int32 -> IO ()

main = do
  hwnd <- platformCreateWindow
  hdc <- getDC (Just hwnd)
  egl <- eglInitializeOn (Just hdc) -- Nothing also works
    [[(egl_RenderableType, egl_OpenGLES2)]]
    [(egl_ClientVersion, 2)]
  forkGL (eglResume egl hwnd >> return False) (eglSuspend egl) (eglPostFrame egl)
  putStrLn "hello!"
  putStrLn.show =<< glReadLogs
  putStrLn.show =<< eglScreenDims egl
  --updateWindow hwnd
  allocaMessage pump
  --stopGL
  --unregisterClass clsName hinst

platformCreateWindow = do
  let clsName = mkClassName "My Window Class"
  hinst       <- getModuleHandle Nothing
  whiteBrush  <- getStockBrush wHITE_BRUSH
  curArrow    <- loadCursor Nothing iDC_ARROW
  mAtom       <- registerClass (
      cS_OWNDC, --cS_DBLCLKS,
      hinst,          -- HINSTANCE
      Nothing,        -- Maybe HICON
      Just curArrow,  -- Maybe HCURSOR
      Just whiteBrush,-- Maybe HBRUSH
      Nothing,        -- Maybe LPCTSTR
      clsName)
  
  -- adjustWindowRect V4 wStyle False
  
  --when (isJust mAtom) $ do
  hwnd <- createWindow
      clsName
      "Redbox"
      (wS_THICKFRAME + wS_CAPTION + wS_SYSMENU)
      Nothing
      Nothing
      (Just 600)
      (Just 600)
      Nothing
      Nothing
      hinst
      wndProc
  
  showWindow hwnd sW_SHOWNORMAL
  return hwnd

pump lpmsg = do
  fContinue <- getMessage lpmsg Nothing
  when fContinue $ do
    translateMessage lpmsg
    dispatchMessage lpmsg
    pump lpmsg

wndProc
  :: HWND
  -> WindowMessage
  -> WPARAM
  -> LPARAM
  -> IO LRESULT
wndProc hwnd wm wp lp
  | wm == wM_KEYDOWN     = doFinish
  | wm == wM_LBUTTONDOWN = doFinish
  | wm == wM_DESTROY     = postQuitMessage 0 >> return 0
  | wm == wM_PAINT       = onPaint
  | otherwise            = defWindowProc (Just hwnd) wm wp lp
  where
    doFinish = sendMessage hwnd wM_CLOSE 1 0 >> return 0
    onPaint  = allocaPAINTSTRUCT $ \ lpps -> do
      -- OpenGL ES Rendering
      runGL $ do
        putStrLn . show $ [glVersion, glRenderer, glVendor, glShadingLanguageVersion, show glExtensions]
        clear [clearColor 1 0 0 1] (colorBuffer)
      endFrameGL
      
      -- GDI Rendering
      hdc <- beginPaint hwnd lpps
      render hwnd hdc
      endPaint hwnd lpps
      
      --invalidateRect (Just ) (const (return ()))
      return 0

render :: HWND -> HDC -> IO ()
render hwnd hdc = do
  setBkMode hdc tRANSPARENT
  setTextColor hdc $ rgb 0 0 0
  textOut hdc 5 5 "hello world!"

