# Appendix

## Appendix A: Additions for ISM-v2<a name="ISMv-2"></a>
The ISM-v2 adds edge information to the mesh file.

The first line of the mesh file will state that fact, that is, if the first line is ISM-V2 then it will have the edge information.

Line 1:

 	ISM-V2

The second line now also includes the number of edges in the mesh as follows:

	#nodes, #edges, #elements, polynomial order of boundary edges

The edges are read immediately after the nodes. For each edge the following are listed:

	start node ID, end node ID, element ID on left, element ID on right, side of left element, side of right element

These are the quantities that are computed in Alg. 148 of "Implementing Spectral Methods". If the edge is a boundary edge, then the second side element will be ID = 0 and the side of that element will be 0. If the sides have indices that increase in opposite directions, then the last column in the data will be negative.￼
## Appendix B: Summary of Boundary Curve Definitions<a name="BCSummary"></a>

Defining a parametric equation:

	   \begin{PARAMETRIC_EQUATION_CURVE}
		 name = <name>
		 xEqn = x(t) = <x-equation>
		 yEqn = y(t) = <y-equation>
		 zEqn = z(t) = 0.0
	 \end{PARAMETRIC_EQUATION_CURVE}

Defining a Spline:

	\begin{SPLINE_CURVE}
	   name = <name>
	   nKnots = # of nodes
        \begin{SPLINE_DATA}
	      t x y z
	      .
	      .
	      .
        \end{SPLINE_DATA}
	 \end{SPLINE_CURVE}

Defining a Straight Line

      \begin{END_POINTS_LINE}
	 	name   = <name>
	 	xStart = [x,y,0]
           xEnd   = [x,y,0]
      \end{END_POINTS_LINE}

Defining a Circular Arc

      \begin{CIRCULAR_ARC}
	 	name 		 = <name>
		units 	 =degrees/radians(Optional.Default:radians)
	 	center 	 = [x,y,0]
           radius 	 = r
		start angle = Tstart
		end angle   = Tend
      \end{CIRCULAR_ARC}

Chaining curves

	\begin{CHAIN}
		name = <Chain Name>
		First curve definition
		Second curve definition
		...
		Last curve definition
	\end{CHAIN}￼
## Appendix C: Summary of Model Definition Blocks<a name="ModelSummary"></a>

No inner boundaries:

	   \begin{MODEL}
	 	\begin{OUTER_BOUNDARY}
			First curve definition
			Second curve definition
			...
			Last curve definition
	 	\end{OUTER_BOUNDARY}
	\end{MODEL}

No outer boundaries:

	   \begin{MODEL}
	 	\begin{INNER_BOUNDARIES}
			First chain definition
			Second chain definition
			...
			Last chain definition
	 	\end{INNER_BOUNDARIES}
	\end{MODEL}

Both inner and outer boundaries:

	   \begin{MODEL}
	 	\begin{OUTER_BOUNDARY}
			First curve definition
			Second curve definition
			...
			Last curve definition
	 	\end{OUTER_BOUNDARY}
	 	\begin{INNER_BOUNDARIES}
			First chain definition
			Second chain definition
			...
			Last chain definition
	 	\end{INNER_BOUNDARIES}
	\end{MODEL}

## Appendix D: Summary of the Control Block<a name="ControlSummary"></a>

The control block (required):

	\begin{CONTROL_INPUT}
		...
	\end{CONTROL_INPUT}

The run parameters (required):

	\begin{RUN_PARAMETERS}
	  mesh file name   = <pathToMeshFile>
	  plot file name   = <pathToPlotFile>
	  stats file name  = <pathToStatsFile> **or** none
	  mesh file format = ISM **or** ISM-v2
	  polynomial order = Boundary polynomial order
	  plot file format = skeleton **or** sem
	\end{RUN_PARAMETERS}

To specify the background grid (required):

	\begin{BACKGROUND_GRID}
	  background grid size = [x,y,0.0]
	\end{BACKGROUND_GRID}

if there is an outer boundary curve in the model. If there is no outer boundary, just an implied box, then use

	\begin{BACKGROUND_GRID}
	   x0 = [xLeft, yBottom, 0.0]
	   dx = [dx, dy, 0.0]
	   N  = [Nx,nY,0]
	\end{BACKGROUND_GRID}

Smoothing is recommended (highly!)

	\begin{SPRING_SMOOTHER}
	  smoothing            = ON **or** OFF
	  smoothing type       = LinearAndCrossbarSpring **or* LinearSpring
	  number of iterations = typically 20-30
	\end{SPRING_SMOOTHER}

If manual local refinement is desired, include

	\begin{REFINEMENT_REGIONS}
		...
	\END{REFINEMENT_REGIONS}

with blocks of the types

       \begin{REFINEMENT_CENTER}
          type = smooth **or** sharp
          x0   = [xCenter,-yCenter,0.0]
          h    = mesh size
          w    = radial extent
      \end{REFINEMENT_CENTER}

       \begin{REFINEMENT_LINE}
          type = smooth **or** sharp
          x0   = [xStart,yStart,0.0]
          x1   = [xEnd,yEnd,0.0]
          h    = mesh size
          w    = width of line
       \end{REFINEMENT_LINE}

To generate 3D meshes, add an extrusion algorithm, either

	\begin{SIMPLE_EXTRUSION}
	  direction          = 1 (=x), 2 (=y), 3 (=z)
	  height             = height of extrusion
	  subdivisions       = how many elements in the extrusion direction
	  start surface name = name of start surface
	  end surface name   = name of end surface
	\end{SIMPLE_EXTRUSION}

or to sweep-rotate a 2D mesh,


    \begin{SIMPLE_ROTATION}
      direction             = 1 (=x), 2 (=y), 3 (=z) = rotation axis
      rotation angle factor =  fraction of pi
      subdivisions          = number of elements in direction
      start surface name    = name of start surface
      end surface name      = name of end surface
    \end{SIMPLE_ROTATION}

or to sweep along a curve,

    \begin{SWEEP_ALONG_CURVE}
      algorithm                = Hanson (optional)
      subdivisions per segment = Subdivisions for each curve in sweep curve chain
      start surface name       = name of start surface
      end surface name         = name of end surface
    \end{SWEEP_ALONG_CURVE}

For the sweep-curve, add the curve to the model:

	\begin{SWEEP_CURVE}
		... Curve chain ...
	\end{SWEEP_CURVE}

and if scaling along the sweep is desired, also add

      \begin{SWEEP_SCALE_FACTOR}
      	... chain of PARAMETRIC_EQUATIONs
      \end{SWEEP_SCALE_FACTOR}

to the model.

If the SIMPLE_EXTRUSION is used, bottom topography can be optionally added to the model

	\begin{TOPOGRAPHY}
		eqn = f(x,y) = some function of (x,y) as an equation
	\end{TOPOGRAPHY}

## Appendix E: ABAQUS mesh file format<a name="ABAQUS"></a>

The [Abaqus mesh file format](https://abaqus-docs.mit.edu/2017/English/SIMACAEMODRefMap/simamod-c-model.htm) is common in the finite element community. The ABAQUS mesh file should use the `.inp` extension. The standard Abaqus format includes a list of node data and a list of element connectivity information. In this sense it is similar to the ISM format without high-order boundary information. The standard Abaqus format encodes a straight sided (linear) mesh.

Herein, we decribe an extended version to the ABAQUS format, divided into two parts. The first parts includes the standard Abaqus node and element lists. The second part encodes the high-order boundary information and naming information created by HOHQMesh. This second set of information is output in such a way that it will be ignored by standard ABAQUS file parsers, but the information is available if desired. For example, a `.inp` mesh file created by HOHQMesh can be parsed into [Trixi.jl](https://github.com/trixi-framework/Trixi.jl) to create high-order curved AMR meshes.

First, we describe the linear mesh skeleton encoded in the standard Abaqus format. For this example we use a quadrilateral mesh with 122 corner nodes and 103 elements:

	*Heading
 	 File created by HOHQMesh
	*NODE
	1, x1, y1, z1
	2, x2, y2, z2
	.
	.
	.
	122, x122, y122, z122
	*ELEMENT, type=CPS4, ELSET=Surface1
	1, 1, 2, 9, 8
	2, 2, 3, 10, 9
	.
	.
	.
	103, 7, 122, 87, 8

Note that the first column in the node or element list is used for indexing purposes. The four other indices in each line of the `*ELEMENT` list
are the corner IDs that dictate the element connectivity. These corner IDs are listed to guarantee right-handed element just as with the ISM format.
The Abaqus element type `CPS4` corresponds to a quadrilateral element. For the three-dimensional variant of this mesh file output we use

	*ELEMENT, type=C3D8, ELSET=Volume1

where `C3D8` corresponds to a hexahedron.

The standard Abaqus file format can be used to create a straight-sided mesh. The high-order boundary information and curvature
generated by HOHQMesh is output in the second portion of this mesh file. We demarcate between the two portions of the mesh file with the comment
line

	** ***** HOHQMesh boundary information ***** **

In the Abaqus format anything prefaced with `** ` is treated as a comment and is ignored by an ABAQUS file parser. Therefore, all the HOHQMesh
information output in the second portion of the mesh file is prefaced with `** ` to act as an Abaqus comment. After the above comment line the mesh
file gives the polynomial degree of the boundary curves in the mesh. Then, this mesh file format closely resembles the ISM format albeit slightly reordered. First, there is a list of the element connectivity, curved edge checks, and polynomial interpolant information after which comes the list of boundary names.

The order of the boundary names is a final difference in the mesh format. The Abaqus format and libraries that use it (e.g. [`p4est`](https://github.com/cburstedde/p4est)) require the boundary labels in a particular order. In general, we can think of a quadrilateral element to have sides labeled

                          +y
                   -----------------
                   |               |
                   | ^ y           |
                -x | |             | +x
                   | |             |
                   | ---> x        |
                   -----------------
                           -y

For this mesh file output the boundary labels are reordered to adopt the order convention of `-x +x -y +y`. For comparison, the default HOHQMesh
ordering used by ISM or ISM-v2 gives the boundary labels in the order `-y +x +y -x`. Similarly, the boundary names are reordered for the 3D HOHQMesh output to adopt the convention `-x +x -y +y -z +z` compared to the ISM formatting of 3D boundary names which is `-y +y -z +x +z -x`.