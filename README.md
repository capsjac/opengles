OpenGL ES: Open Graphics API for Embeded Systems
================================================

[![Hackage](https://img.shields.io/hackage/v/opengles.svg)](https://hackage.haskell.org/package/opengles)

`opengles` is a Haskell wrapper library around OpenGL, OpenGL ES and EGL.
Made complicated OpenGL APIs easy yet keep flexible enough.
Resulting binary size is relatively small so that apps launches faster.
Works on both desktop and mobile.

Documentation is available through [hackage](https://hackage.haskell.org/package/opengles)
for the current and preceding releases.

Props
-----

  Garbage collection for GPU objects
  Pointer operation free
  Simplified nearly state-free rendering API
  Compile-time type check of buffers, textures, programs and so on
  Run on Linux (Mesa), Windows and Android (OS X / iOS contribution is welcome)
  Vertex Array Object Extension with fallback
  Multi Draw Extension with fallback

Cons
----

  Poor documentation and lack of tutorial
  No legacy APIs
  No uncommon extensions
  No support for most of glGet* functionality that harm performance


Examples
--------

See [`examples/`](https://github.com/capsjac/opengles/examples) directory.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or email.
