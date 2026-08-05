# Boundary Error Controlled Adaptive Meshing

!!! danger "Caution"
    The error controlled adaptive meshing is a new addition to HOHQMesh, and is still under development.
    It should be used only with some understanding of how it works, and the assumptions involved.<br>
    Be prepared for failure in the meshing or that a chosen error tolerance may not be achieved, and let us know when this occurs.

HOHQMesh automatically sizes elements according to a number of criteria, including the local radius of curvature of the boundary curves, the distance between curves, and user-defined refinement regions.

It now can also adaptively mesh a model to provide optimal boundary approximations to the model curves with user-specified smoothness to within a user-defined tolerance. Adaptively meshing the model can lead to more efficient meshes with fewer elements and larger element sizes than the default, in addition to ensuring that the boundary approximations written out to the mesh file are accurate to within a desired tolerance. A description of the mathematics behind the optimal curve approximations, how it works, and its limitations can be found in the developer documentation, [here](boundary-curve-optimization-details.md).

## How to Control the Boundary Errors
Error control is added chain-by-chain in the model by telling HOHQMesh what norm to optimize ($\mathbb L^2$ or $\mathbb H^1$), the error tolerance, and to what derivatives the resulting boundary approximation will be smooth. The syntax is

	\begin{CHAIN}
		optimize = L2Norm OR H1Norm OR none
		tolerance = <real value>
		continuity = <integer value> = highest derivative to be made smooth
		connect = <see below>
		.
		.
		.
	\end{CHAIN}

An `OUTER_BOUNDARY` implicitly includes a chain. So, the optimization keywords can directly
be added to a `\begin{OUTER_BOUNDARY} ... \end{OUTER_BOUNDARY}` block.
An example control file that uses the error based adaptive mesh refinement is available in
[`Examples/2D/BlobAdapt/BlobAdapt.control`](https://github.com/trixi-framework/HOHQMesh/blob/main/Examples/2D/BlobAdapt/BlobAdapt.control).

HOHQMesh will then find the best polynomial approximation of each curve in the chain to the order defined by `polynomial order` in the `RUN_PARAMETERS` section of the `CONTROL_INPUT` [block](the-control-input.md#RunParameters).

The options are:

1. **optimize:** Specifies the norm to be minimized. For convenience, `none` can be used to turn it off.
2. **tolerance:** The accuracy to which the boundaries are to be approximated.
3. **continuity:** The derivatives to which the approximation will be constrained. For example, if second derivative continuity is to be enforced, choose `continuity = 2`. If zero, then the approximation is simply continuous.
4. **connect:** This *optional* parameter allows one to optimize across segments of a chain. By default, optimization is done curve by curve within the chain, since usually there will be discontinuities (e.g. corners) between the curves. If it is desired to define a more global approximation across multiple curves, include the `connect` key.

 The syntax is the following:

		connect = crv_1-crv_2,crv_3-crv_4,...

 where the `crv_n` are the index of the curves in the chain. For example, if a chain contains ten curves and optimization is requested across the third and fourth curves in the list and across the sixth through ninth in the list, then

		connect = 3-4,6-9

**Note Well:**

* Optimized adaptive meshing is a feature under active development, with all caveats applied.
* There is no reason to optimize straight line or low order polynomial chains, which is why optimization is implemented chain-by-chain.
* Optimization of the mesh is an expensive process. It will take significantly longer to generate a mesh, especially if the error tolerances are low.
* If the tolerance is chosen too low, it is possible to hit the depth limit, or take an inordinate amount of time. Think about how accurate of a mesh is needed before choosing something like $10^{-10}$. It is also possible to choose tolerances inconsistent with internal tolerances used by HOHQMesh.

## Boundary Error Output

Whether or not boundary optimization is chosen, HOHQMesh will write out the maximum $\mathbb L^2$ and $\mathbb H^1$ boundary edge errors. Running the [`Examples/2D/BlobAdapt/BlobAdapt.control`](https://github.com/trixi-framework/HOHQMesh/blob/main/Examples/2D/BlobAdapt/BlobAdapt.control) example and requesting an H1Norm tolerance of $10^{-4}$ and first derivative continuity by

	\begin{OUTER_BOUNDARY}
	   optimize   = H1Norm
	   continuity = 1
	   tolerance  = 1.0e-4
	   \begin{PARAMETRIC_EQUATION_CURVE}
		 name = blob
		 xEqn = x(t) = 4*cos(2*pi*t) - 3/5*cos(8*pi*t)^3
		 yEqn = y(t) = 4*sin(2*pi*t) - 0.5*sin(11*pi*t)^2
		 zEqn = z(t) = 0.0
	   \end{PARAMETRIC_EQUATION_CURVE}
	\end{OUTER_BOUNDARY}

produces the output:

    *******************
    2D Mesh Statistics:
    *******************
    Total time             =   0.57661399999999996
    Number of nodes        =          570
    Number of Edges        =         1059
    Number of Elements     =          490
    Number of Subdivisions =            4

	 Mesh Quality:
         Measure         Minimum         Maximum         Average  Acceptable Low Acceptable High       Reference
     Signed Area      0.00122001      0.82263252      0.10243554      0.00000000    999.99900000      1.00000000
    Aspect Ratio      1.02370049      2.84878712      1.32371594      1.00000000    999.99900000      1.00000000
       Condition      1.00112287      3.80020102      1.21708413      1.00000000      4.00000000      1.00000000
      Edge Ratio      1.02962316      6.00757565      1.63781443      1.00000000      4.00000000      1.00000000
        Jacobian      0.00049569      0.75891397      0.07875994      0.00000000    999.99900000      1.00000000
    Minimum Angle     34.57990654     87.89988997     68.98664291     40.00000000     90.00000000     90.00000000
    Maximum Angle     91.84435669    150.95227640    114.13256251     90.00000000    135.00000000     90.00000000
       Area Sign      1.00000000      1.00000000      1.00000000      1.00000000      1.00000000      1.00000000

	 Boundary Error Quality:
                   Boundary Name    Max L2 Error     Max H1Error
                  Outer Boundary  5.13545249E-08  5.45735801E-05

In addition to the usual element quality measures, one sees that the $\mathbb H^1$ error is indeed bounded by the requested tolerance. If there is more than one boundary, they are listed by boundary name.

HOHQMesh can optionally write out the boundary approximation errors to files for plotting and later analysis, whether or not the adaptive procedure is requested. Two files can be written prepended with the name of the control file. One contains the point-wise error along each boundary as a function of the chain curve parameter. The other contains the integrated ($\mathbb L^2$ or $\mathbb H^1$ norms), grouped by chain. To have these files written, include the following key in the [RUN_PARAMETERS](the-control-input.md#RunParameters) block:

		error file name = <path_to_desired_file>

Choose the name to be `none` to turn off printing out the errors, or simply delete the line from the file.