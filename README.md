# Gloria
Gloria is a simple 2D game engine built on top of [SDL2](https://github.com/jonathanBieler/SimpleDirectMediaLayer.jl) and written in pure Julia.

# Usage

Please consult the `examples/` folder for examples of how to use Gloria.

# Engine design

Gloria internally runs multiple asynchronous tasks/threads to separate event handling (mouse and keyboard), update calls and rendering.  These tasks call `onevent!`, `update!` and `render!`, respectively, on objects put into the game world.  Using multiple dispatch, behaviour can be added to objects, just as in an object-oriented design.  The engine also comes with a (slightly experimental) rigid body collision system, allowing for game physics without external dependencies.
