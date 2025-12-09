WXYZ
====

A Wayland compositor in the spirit of XMonad.

This project builds off of the TinyWL reference compositor, and aims to get to a
reasonable usable, incrementally, while keeping the implementation as simple and
stupid as necessary.

This project is inspired by [tiny-wlhs](https://github.com/l-Shane-l/tiny-wlhs).
The main insight over that project is that an event stream is architecturally preferable to C callbacks.
It makes implementing a monadic interface easy, without multi-layered Haskell-C-Haskell sandwiches.

Our primary goal is to reach a basic level of usability, while still being
XMonad-like, without too much bike-shedding. At the same time we aim to be
somewhat sensible architecturally.
This means that:

1.  we will avoid anything fancy and complicated, such as auto-generation of
    Haskell binding to WLRoots, implementing a Haskell version of wayland-scanner,
    and anything that can be construed as remotely complicated

2.  If the fastest way to get some protocol working is to have something
    implemented in C, we will go ahead with that for now.
    At the same time, the C code should be unobstructive to a sensible
    Haskell based configuration.

3.  We will use design decisions from XMonad as a reference.
    In the long term, I do not believe that this will make sense, since
    Wayland and X are different beasts. We may of course have a compatibility
    layer in place to allow using things from xmonad-contrib.

    There are also somethings that I don't think make sense to be include in
    the core monad, e.g. workspaces could easily be implemented as a layout,
    allowing different implementations.


Check list
==========

Bootstrap
---------

These are features I'd like to see before declaring victory. Once these are
available I'd likely be able to use this project as my daily driver.

- [x] Tiling using XMonad layouts
- [ ] Floating
- [ ] Layers: Support clients such as Waybar, bemenu, etc
- [ ] Startup action
- [ ] Fullscreen
- [ ] Clipboard and selection functionality, clipboard managers
- [ ] Basic multiple outputs support
- [ ] Screen sharing
- [ ] Mouse bindings
- [ ] Lockscreen

Cleanup
-------

- [ ] Fix StackSet to reflect Wayland.

      Since Wayland has *real* multi-outputs, floating windows must be
      associated with an output. We must also handle the case where
      there are no screens. I think it may make sense to move handling
      of floating windows to layouts.

      It may also make sense to move Workspaces out of core functionality
      and handle them elsewhere as well. This could allow us to, for example
      split large monitors into multiple screens.

- [ ] Modularize components: Currently things are generally placed into similar
      files as in XMonad. Some of these are quite large and diverse.
      We should split these up and reorganize them.

- [ ] Reconsider type names e.g. Window vs TopLevel

- [ ] Expand C event queue to a FIFO. This will stop the crash on during
      shutdown.


Other features
--------------

- [ ] Haskell-side decoration support
- [ ] Input configuration
- [ ] Output configuration
