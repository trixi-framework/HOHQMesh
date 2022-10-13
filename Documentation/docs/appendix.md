# Appendix


## Appendix A: Summary of Boundary Curve Definitions<a name="BCSummary"></a>

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
In one of the few cases where the order of keywords is important, the `nKnots` definition must precede the `\begin{SPLINE_DATA}` block.

Alternatively,

	\begin{SPLINE_CURVE}
		name = <name>
		file = <pathToDataFile>
	\end{SPLINE_CURVE}

The data file will have the number of nodes as the first line, followed by the data, e.g.

		9
		0.000000000000000 -3.50000000000000  3.50000000000000 0.0
		3.846153846153846E-002 -3.20000000000000  5.00000000000 0.0
		7.692307692307693E-002 -2.00000000000000  6.00000000000 0.0
		0.769230769230769  0.000000000000000 -1.00000000000000 0.0
		0.807692307692308 -1.00000000000000 -1.00000000000000 0.0
		0.846153846153846 -2.00000000000000 -0.800000000000000 0.0
		0.884615384615385 -2.50000000000000  0.000000000000000 0.0
		0.923076923076923 -3.00000000000000  1.00000000000000 0.0
		1.00000000000000 -3.50000000000000  3.50000000000000 0.0

Defining a Straight Line:

      \begin{END_POINTS_LINE}
	 	name   = <name>
	 	xStart = [x,y,0]
           xEnd   = [x,y,0]
      \end{END_POINTS_LINE}

Defining a Circular Arc:

      \begin{CIRCULAR_ARC}
	 	name 		 = <name>
		units 	 =degrees/radians(Optional.Default:radians)
	 	center 	 = [x,y,0]
           radius 	 = r
		start angle = Tstart
		end angle   = Tend
      \end{CIRCULAR_ARC}

Chaining curves:

	\begin{CHAIN}
		name = <Chain Name>
		First curve definition
		Second curve definition
		...
		Last curve definition
	\end{CHAIN}￼
## Appendix B: Summary of Model Definition Blocks<a name="ModelSummary"></a>

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

## Appendix C: Summary of the Control Block<a name="ControlSummary"></a>

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
		sizing = ON /OR/ OFF
	\end{TOPOGRAPHY}


The `sizing` keyword is optional if no sizing along the topography is desired, or it can be turned `Off` for the same result.

