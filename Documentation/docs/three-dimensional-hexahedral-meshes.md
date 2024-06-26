# Three Dimensional Hexahedral Meshes
HOHQMesh can also generate 3D hexahedral meshes by extruding or sweeping a two dimensional mesh. Topography can also be added to the bottom of the domain either through a functional form or from data supplied through a file. Finally, when bottom topography is present, the mesh along the bottom can be sized according to the bottom curvature.

To tell the mesher that you want a hex mesh, you add an algorithm block to the `CONTROL_INPUT` block for how the 3D extrusion will be done. Currently there are three:

* [Simple extrusion](#Extrusion),
* [Simple rotation](#Rotation), and
* [Sweeping](#Sweeping).

When sweeping is used, the profile can be scaled along the sweep.

One unique feature of HOHQMesh is that [bottom topography](#Topography) can be added when simple extrusion is used to generate the 3D mesh. Furthermore, the mesh can be [sized](#SizingTopography) according to the curvature of the bottom topography.

## Simple Extrusion<a name="Extrusion"></a>

![HalfCircleExtruded](https://user-images.githubusercontent.com/3637659/121807827-18303b80-cc56-11eb-855a-891037168827.png)
<p align = "center"> Fig. 20. Simple Extrusion of a semi-circular quadrilateral mesh</p>

The first hex-meshing algorithm is the `SIMPLE_EXTRUSION` algorithm.

    \begin{SIMPLE_EXTRUSION}
		 direction          = 3
		 height             = 8.0
		 subdivisions       = 8
		 start surface name = bottom
		 end surface name   = top
	 \end{SIMPLE_EXTRUSION}

The direction (where *x* = 1, *y* = 2, z = *3*) says which direction the extrusion is done. Note that even though the initial 2D mesh is in the x-y plane, the quad mesh is rotated to give a hex mesh extruded in the requested direction. The height tells how far to extrude. A name is given to the bottom and top faces created by the extrusion so that boundary conditions can be attached. Otherwise, the names of the faces are given by the 2D curve names.

## Simple Rotation<a name="Rotation"></a>

![IglooAlt](https://user-images.githubusercontent.com/3637659/121807832-1e261c80-cc56-11eb-8c86-9d2a9e07de00.png)
<p align = "center"> Fig. 21. Simple rotation of the mesh in Fig. 3</p>

The second algorithm is the `SIMPLE_ROTATION`, which rotates the two dimensional mesh about an  axis

    \begin{SIMPLE_ROTATION}
       direction             = 1
       rotation angle factor = 1.0
       subdivisions          = 8
       start surface name    = bottom
       end surface name      = top
    \end{SIMPLE_ROTATION}

The rotation angle factor is the fraction of pi over which the quad mesh is rotated. An example is shown above in Fig. 21 of an original two dimensional mesh
and its rotation about the x axis (direction = 1).

## Sweeping<a name="Sweeping"></a>
![Snake](https://user-images.githubusercontent.com/3637659/121807890-588fb980-cc56-11eb-9698-d3efffafed82.png)
<p align = "center"> Fig. 22. Hex mesh generated by sweeping a circular mesh along a curve</p>

 The most general algorithm for generating hex meshes in HOHQMesh is to sweep a two-dimensional mesh along a prescribed curve, `SWEEP_ALONG_CURVE`. To sweep along a curve, one does two things:

1.  Add  a `SWEEP_ALONG_CURVE` block to the `CONTROL_INPUT` block and
2.  Add the curve along which the sweeping is to be done to the `MODEL` block.

There are currently two sweeping algorithms available. The default is a simple rotation algorithm that has no method to counteract twisting of the mesh as it follows the curve. (Think of a roller-coaster that can turn upside down as it follows a curved track.) The default algorithm is exact so will sweep the curve to high order, but will only produce an untwisted mesh if the curve is planar.

The second is a parallel transport algorithm due to Hanson and Ma that keeps arbitrary vector in a particular orientation with respect to its initial direction. The parallel transport approach minimizes the twisting of the hex mesh, but is only second order accurate. [A fourth order algorithm exists and may be implemented in the future.]

To implement sweeping, include a `SWEEP_ALONG_CURVE` block in the `CONTROL_INPUT` block:

    \begin{SWEEP_ALONG_CURVE}
	   algorithm                = Hanson (optional)
       subdivisions per segment = 8
       start surface name       = bottom
       end surface name         = top
    \end{SWEEP_ALONG_CURVE}

The algorithm keyword is optional. If not present, the sweeping will not include the parallel transport correction. Since the sweep curve can be a chain with slope or curvature singularities, the number of subdivisions per segment is defined. This ensures that a singularity occurs along element boundaries so that accuracy is not lost.

The curve itself is defined in the `MODEL` block.

	\begin{SWEEP_CURVE}
		...
	\end{SWEEP_CURVE}

The `SWEEP_CURVE` block implicitly defines a `CHAIN`, like the `OUTER_BOUNDARY` block, and so only needs a list of curves to define the sweep.

## Scaling<a name="Scaling"></a>
![ScaledSigmoid](https://user-images.githubusercontent.com/3637659/121807887-54639c00-cc56-11eb-9474-9d809320af9b.png)
<p align = "center"> Fig. 23. Hex mesh generated by sweeping and scaling along a curve</p>

The mesh can also be scaled in the direction normal to the sweep curve when sweeping is used.
To scale the mesh, add a

	\begin{SWEEP_SCALE_FACTOR}
		...
	\end{SWEEP_SCALE_FACTOR}

block to the `MODEL`. Like the `SWEEP_CURVE` and `OUTER_BOUNDARY` blocks, the `SWEEP_SCALE_FACTOR` block implicitly defines a `CHAIN`. You do not need to have the number chain segments match the number in the `SWEEP_ALONG_CURVE` block, but it is probably best to not introduce slope or curvature singularities except at element interfaces.

The equation for the scaling is scalar `PARAMETRIC_EQUATION` (as opposed to a `PARAMETRIC_EQUATION_CURVE`). It is defined, for example like this:

        \begin{PARAMETRIC_EQUATION}
           eqn = r(t) = 1.0 + 2.5*t*(1-t)
        \end{PARAMETRIC_EQUATION}

## Bottom Topography<a name="Topography"></a>
![Pond](https://user-images.githubusercontent.com/3637659/121807861-40b83580-cc56-11eb-8d97-388924e08dee.png)
<p align = "center"> Fig. 24. Simple Extrusion of a semi-circular mesh with bottom topography</p>

When using the `SIMPLE_EXTRUSION` algorithm, bottom topography can be defined in one of two ways:

* By supplying a functional form, f(x,y)
* By supplying gridded data in a file

### Topography from a Functional Form
The simplest way to define the bottom topography is with an equation in a `TOPOGRAPHY` block, e.g.,

        \begin{TOPOGRAPHY}
           eqn = h(x,y) = x^2 + y^2
        \end{TOPOGRAPHY}

The height function takes two arguments, which are the physical x-y coordinates, unlike the parametric coordinates that define boundary curves. Fig. 24 above shows an example of such a bottom topography.

### Topography from Data
Alternatively, the bottom topography data can be read in from a file, e.g.,

        \begin{TOPOGRAPHY}
           data file = path/to/bottom_data.txt
        \end{TOPOGRAPHY}

From this data a bicubic interpolation is used to compute the bottom topography information.
Currently this strategy of bottom topography extrusion relies on gridded data. The data file is assumed to come as separate lists of the x coordinate points, the
y coordinate points, and the z coordinate points where the grid data is ordered slice-by-slice in the y direction. Below a small example is provided to clarify the
data file format

     ! Header with the number of points in the x and y direction
      75 50
     ! x node values
     x1
     ...
     x75
     ! y node values
     y1
     ...
     y50
     ! z node values
     z1,1
     z2,1
     ...
     z75,1
     z1,2
     ...
     z75,2
     ...
     z75,50

![MtStHelens](https://user-images.githubusercontent.com/25242486/157440694-b8f1d9d5-adbf-4f5b-9de0-f89f675c584f.png)
<p align = "center"> Fig. 25. Simple Extrusion of a rectangular mesh with a mountain bottom topography read in from a file</p>

### Sizing the Mesh along Bottom Topography<a name="SizingTopography"></a>
![SeaMountWithSizing](https://user-images.githubusercontent.com/85404032/160676054-d5f12eee-4f2b-4674-8861-1d131e9e8493.png)
<p align = "center"> Fig. 26. Simple Extrusion of a semi-circular mesh with a bottom topography and local refinement depending on its curvature</p>

Finally, the grid can be sized along the bottom by adding the command

		sizing = ON

to any TOPOGRAPHY block, e.g

		\begin{TOPOGRAPHY}
		   data file = path/to/bottom_data.txt
		   sizing    = ON
		\end{TOPOGRAPHY}

It can also be turned `OFF', or the line deleted, to not size the bottom.

The sizer will then size the grid along the bottom according to the curvature of the topography. An example is shown in Fig. 26.
