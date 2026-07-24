# Boundary Error Controlled Adaptive Meshing

!!! danger "Caution"
    The error controlled adaptive meshing is a new addition to HOHQMesh, and is still under development.
    It should be used only with some understanding of how it works, and the assumptions involved.<br>
    Be prepared for failure in the meshing, and let us know when this occurs.

HOHQMesh automatically sizes elements according to a number of criteria, including the local radius of curvature of the boundary curves, the distance between curves, and user-defined refinement regions.

It now can also adaptively mesh a model to provide optimal boundary approximations to the model curves with user-specified smoothness to within a user-defined tolerance. Adaptively meshing the model can lead to more efficient meshes with fewer elements and larger element sizes than the default, in addition to ensuring that the boundary approximations written out to the mesh file are accurate to within a desired tolerance. A description of the mathematics behind the optimal curve approximations, how it works, and its limitations can be found in the developer documentation, [here](boundary-curve-optimization-details.md).

## How to Control the Boundary Errors
Error control is added chain-by-chain in the model by telling HOHQMesh what norm to optimize ($\mathbb L^2$ or $\mathbb H^1$), the error tolerance, and to what derivatives the resulting boundary approximation will be smooth. The syntax is

	\begin{CHAIN}
		optimize = L2Norm OR H1Norm OR none
		tolerance = <real value>
		continuity = <integer value>
		connect = <see below>
		.
		.
		.
	\end{CHAIN}

An `OUTER_BOUNDARY` implicitly includes a chain. So, the optimization keywords can directly
be added to a `\begin{OUTER_BOUNDARY} ... \end{OUTER_BOUNDARY}` block.

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
HOHQMesh can optionally write out the boundary approximation errors to files for later analysis, whether or not the adaptive procedure is requested. Two files can be written prepended with the name of the control file. One contains the point-wise error along each boundary as a function of the chain curve parameter. The other contains the integrated ($\mathbb L^2$ or $\mathbb H^1$ norms), grouped by chain. To have these files written, include the following key in the [RUN_PARAMETERS](the-control-input.md#RunParameters) block:

		error file name = <path_to_desired_file>

Choose the name to be `none` to turn off printing out the errors, or simply delete the line from the file.