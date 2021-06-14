# The Model
At the present time, HOHQMesh is designed to generate quadrilateral meshes in general two dimensional geometries like those shown below, and extrusions thereof to get three dimensional hex meshes.

![Meshables](https://user-images.githubusercontent.com/3637659/121807852-3302b000-cc56-11eb-93a9-e9c2e1b4ede8.png)
<p align = "center"> Fig. 13. Meshable regions</p>

The two dimensional domain to be meshed can be bounded by at most one exterior boundary curve (which can be composed of a chain or curves), as in (a) and (b), above, and any number of interior boundary curves that create holes. For purely external problems, a rectangular outer boundary can be implicitly included, as shown in (c).


## Boundary Curves<a name="BoundaryCurves"></a>

![AllFeatures](https://user-images.githubusercontent.com/3637659/121807794-f3d45f00-cc55-11eb-9284-af5f4eed2c87.png)
<p align = "center"> Fig. 14. A mesh whose model uses all curve types. Three *END_POINTS_LINE*s for the outer triangle. A *SPLINE_CURVE* for the free-form inner boundary, and circles defined by a *PARAMETRIC_EQUATION_CURVE* and by a *CIRCULAR_ARC* curve.</p>

Boundaries are constructed as closed chains of parametrized curves, with the parameter in the interval [0,1], oriented counter-clockwise. The chains can have one or more segments as seen in Fig. 14. In Fig. 14a the outer boundary is constructed from six curves, whereas in Fig. 14b it is bounded by a single one. The inner boundaries in Fig. 14a are a single circle and a square constructed by a chain of four lines.

A curve is defined by a block 

	\begin{curve_type}
		...
	\end{curve_type}

It is given a name so that boundary conditions can be applied segment-by-segment to a chain. 

Currently there are four types of curves that can be defined:

*  Curves defined by equation components. 
*  Cubic spline interpolants of a set of nodal points. 
*  Straight lines between two points.
*  Circular arcs. 

Fig. 6 is an example that uses all four curve-type definitions.

The architecture is designed to easily add curve definitions in the future by creating subclasses of the SMCurveClass.

### The Parametric Equation Curve Definition.<a name="ParametricEqn"></a> 

Curves can be defined by strings that define the equations for the (x,y,z) components of the curve using the *PARAMETRIC\_EQUATION\_CURVE* type. An example block for this kind of curve is

	\begin{PARAMETRIC_EQUATION_CURVE}
		name = circle
		xEqn = x(t) = 14.0*cos(2*pi*t)
		yEqn = y(t) = 14.0*sin(2*pi*t)
		zEqn = z(t) = 0.0
	\end{PARAMETRIC_EQUATION_CURVE}

The first line defines the name, followed by the x- , y- and z- equation definitions. **Right now, only meshes in the x-y plane can be generated, so the z=0 equation must be set this way**. The example block defines a closed circular curve of radius 14 named “circle”. The indenting is optional, as is the ordering of the keys within the block. The keywords are “name”, “xEqn”, etc. and must be spelled correctly or an error will be posted when the model is read in. The zEqn keyword is optional and can be left out.

The equations can be any legal representations of an equation as is standard in most computer languages. The first part, before the equals sign defines the parameter variable, in this case, t. On the right hand side is the formula that defines the curve. Exponentiation is defined as in BASIC, like t^2. For convenience, the constant **pi** is defined. Like BASIC, literals are defined as double precision values. There are no integer quantities. Standard functions like sin, cos, tan, atan, log, log10, exp, etc. are also available for use.

### The Spline Curve Definition<a name="Spline"></a>
The second type of curve is the SplineCurve type, which fits a cubic spline to a set of knots at given parameter values. The parameterization does not have to be uniform. An example of a spline-defined curve is

	\begin{SPLINE_CURVE}
		name = SplineBoundaryCurve
		nKnots = 9
		\begin{SPLINE_DATA}
			0.000000000000000 -3.50000000000000  3.50000000000000 0.0
			3.846153846153846E-002 -3.20000000000000  5.00000000000 0.0
			7.692307692307693E-002 -2.00000000000000  6.00000000000 0.0
			0.769230769230769  0.000000000000000 -1.00000000000000 0.0
			0.807692307692308 -1.00000000000000 -1.00000000000000 0.0
			0.846153846153846 -2.00000000000000 -0.800000000000000 0.0
			0.884615384615385 -2.50000000000000  0.000000000000000 0.0
			0.923076923076923 -3.00000000000000  1.00000000000000 0.0
			1.00000000000000 -3.50000000000000  3.50000000000000 0.0
		\end{SPLINE_DATA}
	\end{SPLINE_CURVE}

As before, the first line after the \begin is the name of the curve. It is followed by the number of nodes in the spline. The data columns that follow are the nodes given by tj,xj,yj,zj. This particular spline is closed, so the location of the last node is the same as the first. Again, the zj values must currently be zero to ensure that curves are in the x-y plane.

### Endpoints Line Definition<a name="EndPointsLine"></a>
The next type of curve is the *END\_POINTS\_LINE* type that takes two end points and puts a straight line between them. An example is

	\begin{END_POINTS_LINE}
		name   = B1
		xStart = [0.0,1.0,0.0]
		xEnd   = [2.0,1.0,0.0]
	\end{END_POINTS_LINE}

### Circular Arc Curve<a name="CircularArc"></a>

The final type of curve defines a circular arc. The angles can be defined either in terms of degrees or radians. If the (optional) units keyword is not included, the default is radians.

	\begin{CIRCULAR_ARC}
		name        = circle
		units       = degrees
		center      = [0.0, 0,0,0.0]
		radius      = 4.0
		start angle = 0.0
		end angle   = 180.0
	\end{CIRCULAR_ARC}

## Boundary Chains<a name="Chains"></a>

To allow complex boundary curves and to allow different portions of a boundary to have different boundary conditions applied, curves can be chained together into a closed curve. A chain is defined by curves specified (in order) within a 

	\begin{CHAIN}
	...
	\end{CHAIN} 

block. Any number of curves can be chained together. The chain itself is also given a name. An example of a chain that defines the boundary of a unit square is

	\begin{CHAIN}
		name = UnitSquare
		\begin{PARAMETRIC_EQUATION_CURVE}
			name = bottom
			xEqn = f(t) = t
			yEqn = f(t) = 0
			zEqn = f(t) = 0
		\end{PARAMETRIC_EQUATION_CURVE}

		\begin{PARAMETRIC_EQUATION_CURVE}
			name = right
			xEqn = f(t) = 1
			yEqn = f(t) = t
			zEqn = f(t) = 0
		\end{PARAMETRIC_EQUATION_CURVE}

		\begin{PARAMETRIC_EQUATION_CURVE}
			name = top
			xEqn = f(t) = 1-t
			yEqn = f(t) = 1
			zEqn = f(t) = 0
		\end{PARAMETRIC_EQUATION_CURVE}

		\begin{PARAMETRIC_EQUATION_CURVE}
			name = bottom
			xEqn = f(t) = 0
			yEqn = f(t) = 1-t
			zEqn = f(t) = 0
		\end{PARAMETRIC_EQUATION_CURVE}
	\end{CHAIN}

Again, the indentation is for readability only, as is the line spacing between the blocks. (Blank lines and lines starting with “%” are ignored.) Also remember that the chain is defined counter-clockwise, and the curves within the chain must be ordered and oriented properly. Chains cannot be chained together.

## The Model Definition<a name="TheModel"></a>
The model (there is only one) defines the region that is to be meshed. It is marked by 

	\begin{MODEL}
	...
	\end{MODEL}

The model contains at most one outer boundary chain and any number of inner boundary chains. The outer boundary chain (if there is one) is defined by

	\begin{OUTER_BOUNDARY}
	...
	\end{OUTER_BOUNDARY}

Within the *OUTER_BOUNDARY* block is a list of boundary curves that form a chain. There is no need to explicitly chain (by way of \begin{CHAIN}...\end{CHAIN}) the curves for the outer boundary, as that is implied.

Inner boundaries (if any) are defined within

	\begin{INNER_BOUNDARIES}
	...
	\end{INNER_BOUNDARIES}

Within this block one defines as many CHAINs as there are inner boundaries. **Each inner boundary must be defined within a chain**. (Even if there is only one curve in the chain.) The order of the CHAINs is not important. 

As an example, the following defines a model that has a single circular outer boundary and three inner circular boundaries. As usual, indentation is for the reader’s eyes only. Note that *between* the blocks, comments can be inserted starting with “%”.


	\begin{MODEL}
		\begin{OUTER_BOUNDARY}
			\begin{PARAMETRIC_EQUATION_CURVE}
				name = outer
				xEqn = x(t) = 14.0*cos(2*pi*t)
				yEqn = y(t) = 14.0*sin(2*pi*t)
				zEqn = z(t) = 0.0
			\end{PARAMETRIC_EQUATION_CURVE}
		\end{OUTER_BOUNDARY}
	%
	%	Inner boundaries, if any, are any number of chains
	%	of curves.
	% 
		\begin{INNER_BOUNDARIES}
			\begin{CHAIN}
				name = Boundary 1
				\begin{PARAMETRIC_EQUATION_CURVE}
					name = Circle1
					xEqn = f(t) = -10.25 + 0.2*cos(2*pi*t)
					yEqn = f(t) = 3.0 + 0.2*sin(2*pi*t)
					zEqn = z(t) = 0.0
				\end{PARAMETRIC_EQUATION_CURVE}
			\end{CHAIN}

		   \begin{CHAIN}
			name = Boundary 2
 			\begin{PARAMETRIC_EQUATION_CURVE}
				name = Circle2
				xEqn = f(t) = -5.1 + 1.0*cos(2*pi*t)
				yEqn = f(t) = 1.0*sin(2*pi*t) - 4.1
				zEqn = z(t) = 0.0
			\end{PARAMETRIC_EQUATION_CURVE}
		   \end{CHAIN}

		   \begin{CHAIN}
		     name = Boundary 3
	            \begin{PARAMETRIC_EQUATION_CURVE}
				name = Circle3
				xEqn = f(t) = -12.0 + 0.5*cos(2*pi*t)
				yEqn = f(t) = 0.5*sin(2*pi*t) - 0.5
				zEqn = z(t) = 0.0
		      \end{PARAMETRIC_EQUATION_CURVE}
	   		\end{CHAIN}
		\end{INNER_BOUNDARIES}
	\end{MODEL}
