# Boundary Error Optimized Adaptive Meshing
HOHQMesh automatically sizes elements according to a number of criteria, including the local radius of curvature of the boundary curves, the distance between curves, and user defined refinement regions.

It now can also adaptively mesh a model to provide optimal boundary approximations to the model curves with user specified smoothness to within a user defined tolerance. Adaptively meshing the model can lead to more efficient meshes with fewer elements and larger element sizes than the default, in addition to ensuring that the boundary approximations written out to the mesh file are accurate to within a desired tolerance. A description of the mathematics behind the optimal curve approximations, how it works, and its limitations can be found in the developer documentation, here.

Error control is added chain-by-chain by telling HOHQMesh what norm to optimize ($L^2$ or $H^1), the error tolerance, and to what derivatives the resulting boundary approximation will be smooth. The syntax is

	\begin{CHAIN}
		optimization = L2Norm OR H1Norm or none
		tolerance = <real value>
		continuity = <integer value>
		connect = <see below>
		.
		.
		.
	\end{CHAIN}

HOHQMesh will then find the best polynomial approximation of each curve in the chain to the order defined by `polynomial order` in the `RUN_PARAMETERS` section of the `CONTROL_INPUT` block. 

The options are

1. **optimization:** Specifies the norm to be minimized. For convenience, *none* can be used to turn it off.
2. **tolerance:** The accuracy to which the boundaries are to be approximated.
3. **continuity:** The derivatives to which the approximation will be constrained. For example, if second derivative continuity is to be enforced, choose *continuity = 2*, if zero, then the approximation is simply continuous.
4. **connect:** This *optional* parameter allows one to optimize across segments of a chain. By default, optimization is done curve by curve within the chain, since usually there will be discontiuities (e.g. corners) between the curves. If it is desired to define a more global approximation across multiple curves, include the `connect` key.

 The syntax is the following:
 
		connect = crv_1-crv_2,crv_3-crv_4,...
 where crv_n

where the `crv_n` are the index of the curves in the chain. For example, if a chain contains ten curves and optimization is requested across the third and fourth curves in the list and across the sixth though ninth in the list, then

		connect = 3-4,6-9

**Notes:**

* Optimized adaptive meshing is a feature under active development, with all caveats applied.
* There is no reason to optimize straight line or low order polynomial curves, which is why optimization is implemented chain-by-chain.
* Optimization of the mesh is an expensive process. It will take significantly longer to generate a mesh, especially if the error tolerances are low.
* 