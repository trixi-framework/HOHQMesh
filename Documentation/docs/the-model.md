# The Model
At the present time, HOHQMesh is designed to generate quadrilateral meshes in general two dimensional geometries like those shown below in Fig. 15, and extrusions thereof to get three dimensional hex meshes.

![Meshables](https://user-images.githubusercontent.com/3637659/121807852-3302b000-cc56-11eb-93a9-e9c2e1b4ede8.png)
<p align = "center"> Fig. 15. Meshable regions</p>

The two dimensional domain to be meshed can be bounded by at most one exterior boundary curve (which can be composed of a chain of curves), as in 15(a) and 15(b), above, and any number of interior boundary curves that either create holes or interfaces along which element edges are forced to lie. For purely external problems, a rectangular outer boundary can be implicitly included, as shown in 15(c).

If no model block is included at all, then a purely Cartesian mesh will be created using parameters set in the control file.

As an example, the model in Fig. 16 has one outer boundary - the outer triangle - and three inner boundaries whose interiors are exterior to the mesh.

![AllFeatures](https://user-images.githubusercontent.com/3637659/121807794-f3d45f00-cc55-11eb-9284-af5f4eed2c87.png)
<p align = "center"> Fig. 16. A mesh whose model uses all curve types. Three `END_POINTS_LINE`s for the outer triangle. A `SPLINE_CURVE` for the free-form inner boundary, and circles defined by a `PARAMETRIC_EQUATION_CURVE` and by a `CIRCULAR_ARC` curve (<em>Examples/2D/AllFeatures</em>).</p>

 It is also possible to include interface boundaries whose interiors are also meshed. This allows one to force element boundaries along curves and to assign different material properties to each region bounded by them. The property is named by the innermost boundary in which an element lies. An example of a domain with two interface boundaries is shown in Fig. 17. **Right now, interface curves must be defined in the model definition from outer to inner to properly assign material names**. Thus, the innermost circle in Fig. 17 is defined after its enclosing circle. This restriction can be removed by adding code to test whether a given curve lies within another.

![B&M](https://user-images.githubusercontent.com/25242486/241190479-ba50d797-3bd9-41b1-bc38-03fc86f8c3da.png)
<p align = "center"> Fig. 17. Mesh with interior interfaces bounded by circles.</p>

The mesh generator generates unstructured meshes, and there is no reason in general why a mesh must be symmetric if the model is symmetric. Since symmetry can be a desirable feature, HOHQMesh allows one to define a symmetry boundary about which the mesh will be symmetric by setting boundary names as ":symmetry" (with a colon, so as not to interfere with some other use of the name). The symmetry boundary must be a straight line (no matter how that is defined, see below) and multiple symmetry boundaries must be co-linear. What HOHQMesh does is take a model and mesh it. Then, if one or more of the outer boundary curves are named ":symmetry", that mesh will be reflected about the symmetry line and the result will be a perfectly symmetric mesh. An example is shown in Fig. 18. If the curve designated as ":symmetry" is not straight, or if any of multiple lines so designated are not co-linear, then an error is poseted and the mesh will not be reflected.

![NotREFL](https://github.com/trixi-framework/HOHQMesh/assets/25242486/5e145806-1acb-43c4-b712-ea6eeb703c64)
<p align = "center"> Fig. 18. A mesh (left) reflected about the red boundary line (right).</p>

## Boundary Curves<a name="BoundaryCurves"></a>


Boundaries include the outer boundary, any number of inner boundaries, and interface boundaries that mark material or other separations. No boundary can cross another boundary. Boundary curves are constructed as closed chains of parametrized curves, with the parameter in the interval [0,1], oriented counter-clockwise. The chains can have one or more segments as seen in Fig. 15. In Fig. 15a the outer boundary is constructed from six curves, whereas in Fig. 15b it is bounded by a single one. The inner boundaries in Fig. 15a are a single circle and a square constructed by a chain of four lines. In Fig. 16, the outer triangle is constructed as a chain of straight lines defined as `END_POINTS_LINE`s.

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

Fig. 16 is an example that uses all four curve-type definitions (<em>Examples/2D/AllFeatures</em>).

The architecture is designed for developers to easily add curve definitions in the future by creating subclasses of the SMCurveClass.

### The Parametric Equation Curve Definition.<a name="ParametricEqn"></a>

Curves can be defined by strings that define the equations for the (x,y,z) components of the curve using the `PARAMETRIC_EQUATION_CURVE` type. An example block for this kind of curve is

	\begin{PARAMETRIC_EQUATION_CURVE}
		name = circle
		xEqn = x(t) = 14.0*cos(2*pi*t)
		yEqn = y(t) = 14.0*sin(2*pi*t)
		zEqn = z(t) = 0.0
	\end{PARAMETRIC_EQUATION_CURVE}

The first line defines the name, followed by the x- , y- and z- equation definitions. **Right now, only two-dimensional meshes in the x-y plane can be generated, so the z=0 equation must be set this way**. The example block defines a closed circular curve of radius 14 named “circle”. The indenting is optional, as is the ordering of the keys within the block. The keywords are “name”, “xEqn”, etc. and must be spelled correctly or an error will be posted when the model is read in. The zEqn keyword line is optional and can be left out.

The equations can be any legal representations of an equation as is standard in most computer languages. The first part, before the equals sign defines the parameter variable, in this case, *t*. On the right hand side is the formula that defines the curve. Exponentiation is defined as in BASIC, like *t^2*. For convenience, the constant **pi** is defined. Like BASIC, literals are defined as double precision values. There are no integer quantities. Standard functions like sin, cos, tan, atan, log, log10, exp, etc. are also available for use.

### The Spline Curve Definition<a name="Spline"></a>
The second type of curve is the `SPLINE_CURVE` type, which fits a cubic spline to a set of knots at given parameter values. The parameterization does not have to be uniform. An example of a spline-defined curve is

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

As before, the first line after the \begin is the name of the curve. It is followed by the number of nodes in the spline. The data columns that follow are the nodes given by t<sub>j</sub>,x<sub>j</sub>,y<sub>j</sub>,z<sub>j</sub>. This particular spline is closed, so the location of the last node is the same as the first. Again, the z<sub>j</sub> values must currently be zero to ensure that curves are in the x-y plane.

The spline data can get rather large, so there is an option to read it from an external file. To read from a file, the `SPLINE_CURVE` block is

	\begin{SPLINE_CURVE}
		name = SplineBoundaryCurve
		file = `pathToFile`
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

An example where the curve data is read from a file can be found in Examples/2D/EastCoastUS.

### Endpoints Line Definition<a name="EndPointsLine"></a>
The next type of curve is the `END_POINTS_LINE` type that takes two end points and puts a straight line between them. An example is

	\begin{END_POINTS_LINE}
		name   = B1
		xStart = [0.0,1.0,0.0]
		xEnd   = [2.0,1.0,0.0]
	\end{END_POINTS_LINE}

where the x,y,z values of the point are specified in the array denoted as [x,y,z]. For now, the z component must be 0.0.

### Circular Arc Curve<a name="CircularArc"></a>

Another type of curve defines a circular arc. The angles can be defined either in terms of degrees or radians. If the (optional) *units* keyword is not included, the default is radians.

	\begin{CIRCULAR_ARC}
		name        = circle
		units       = degrees
		center      = [0.0,0.0,0.0]
		radius      = 4.0
		start angle = 0.0
		end angle   = 180.0
	\end{CIRCULAR_ARC}

### Elliptic Arc Curve<a name="EllipticArc"></a>

The final type of curve defines an elliptic arc. The angles can be defined either in terms of degrees or radians. If the (optional) *units* keyword is not included, the default is radians. The x and y radii are specified seperately, and the (optional) *rotation* keyword may be passed to rotate the ellipse counterclockwise.

	\begin{ELLIPTIC_ARC}
		name        = ellipse
		units       = degrees
		center      = [0.0,0.0,0.0]
		xRadius     = 4.0
		yRadius     = 2.0
		start angle = 0.0
		end angle   = 180.0
		rotation    = 45.0
	\end{ELLIPTIC_ARC}

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
The model (there is at most one) defines the region that is to be meshed. It is marked by

	\begin{MODEL}
	...
	\end{MODEL}

If the control file does not contain a model block, a Cartesian mesh will be generated.

The model contains at most one outer boundary chain and any number of inner boundary chains. The outer boundary chain (if there is one) is defined by

	\begin{OUTER_BOUNDARY}
	...
	\end{OUTER_BOUNDARY}

Within the `OUTER_BOUNDARY` block is a list of boundary curves that form a chain. There is no need to explicitly chain (by way of `\begin{CHAIN}`...`\end{CHAIN}`) the curves for the outer boundary, as that is implied.

Inner boundaries (if any) are defined within the block

	\begin{INNER_BOUNDARIES}
	...
	\end{INNER_BOUNDARIES}

Within this block one defines as many curves or `CHAIN`s as there are inner boundaries. The order in which the `CHAIN`s or curves are defined is not important. Use a CHAIN if you want to chain multiple curves together to create a single inner boundary. Outside of a chain, a curve will define a single inner boundary by itself. Note that a standalone curve must close on itself.

Interior interface boundary curves are defined inside the `INTERFACE_BOUNDARIES` block, as if they are interior boundaries,

	\begin{INTERFACE_BOUNDARIES}
	...
	\end{INTERFACE_BOUNDARIES}

Interface boundaries, unlike interior boundaries, do not create holes in model domain or the mesh. _Interface boundaries that are contained (embedded) in other interface boundaries must be defined within the block in order from outer to inner to properly define the material names_. The ordering of interface boundaries that are not embedded in another is not important. (Additions to HOHQMesh to remove this restriction will require code to determine whether or not a given curve is embedded within another, and has not yet been implemented. HOHQMesh essentially uses a painters algorithm to specify the material names.) If interface curves are not being used to delineate material boundaries (i.e. for mesh alignment alone and not with the ISM-MM mesh file format), then the ordering is unimportant.

###Example

As an example, the following defines a model that has a single circular outer boundary and three inner circular boundaries. Two of the curves are defined within a CHAIN (even though there is only a single curve within each). One of them is standalone. Note that *between* the blocks, comments can be inserted starting with “%”. As usual, indentation is for the reader’s eyes only.


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
	%	of curves. Each inner boundary is defined within a CHAIN.
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

 			\begin{PARAMETRIC_EQUATION_CURVE}
				name = Circle2
				xEqn = f(t) = -5.1 + 1.0*cos(2*pi*t)
				yEqn = f(t) = 1.0*sin(2*pi*t) - 4.1
				zEqn = z(t) = 0.0
			\end{PARAMETRIC_EQUATION_CURVE}

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
