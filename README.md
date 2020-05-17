# Gloria
Gloria is a simple game engine build on top of [SDL2](https://github.com/jonathanBieler/SimpleDirectMediaLayer.jl).

It works by updating the state by calling `update!` and draw during `render!`. The state is also modifiable based on events such as keyboard or mouse input. It also comes with a collision system based on multiple dispatch, allowing for game physics without external dependencies like Box2d.

It is currently still in development.  
