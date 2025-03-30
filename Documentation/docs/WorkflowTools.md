# Workflow Tools

Two tools are available to help generate control files from which to generate a mesh with HOHQMesh.

## HOHQMesh.jl

<img src="https://user-images.githubusercontent.com/25242486/174798986-6b900fa8-840c-4c04-bc61-00f0749af1be.png" width="400">

[HOHQMesh.jl](https://trixi-framework.github.io/HOHQMesh.jl/stable/) is a simple graphical front end to HOHQMesh that allows one to draw and edit the geometry, mesh and visualize in one package. It also includes binaries for the mesher so that it is not necessary to compile HOHQMesh oneself. Tutorials on how to use HOHQMesh.jl can be found [here](https://trixi-framework.github.io/HOHQMesh.jl/stable/).

## svg-to-HOHQMesh

<img src="https://github.com/trixi-framework/HOHQMesh_examples/blob/a8610caf1c5047741ee341462cb3e2f9d34471c6/gallery/jack-o-lantern-small.png?raw=true" width="350">

[svg-to-HOHQMesh](https://github.com/FluidNumerics/svg-to-HOHQMesh) is a tool created by [FluidNumerics](https://www.fluidnumerics.com) that enables one to draw the geometry in a program like [inkScape](https://inkscape.org) and then write a control file to be meshed. A demo is included on the [gitHub](https://github.com/FluidNumerics/svg-to-HOHQMesh) site, and on [YouTube](https://www.youtube.com/watch?v=xvZwEHcwYOc). The pumpkin mesh in the HOHQMesh [gallery](https://trixi-framework.github.io/HOHQMesh/Gallery/) was generated with svg-to-HOHQMesh.
