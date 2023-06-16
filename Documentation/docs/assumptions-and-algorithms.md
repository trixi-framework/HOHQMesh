# Assumptions and Algorithms

This is a collection of comments on some of the approximations and assumptions made in developing HOHQMesh. It is not exhaustive. It is also not in any particular order.

1. When checking to see whether or not two curves cross (when reading in the model), actual intersection is not tested. Rather, it looks to see if one or two points end up within the same bounding box. The assumption is that if the curves are that close together then the resolution is not good enough to generate a mesh anyway. See the subroutine [CheckForBoundaryIntersections](https://github.com/trixi-framework/HOHQMesh/blob/6c434128154bada1e036f38a9674ca7a52c47c90/Source/Mesh/MeshBoundaryMethods.f90#L1328) for details.

2. The algorithm for parallel transport along a curve to sweep three dimensional meshes is only second order accurate. A 4th order scheme is published, but not implemented yet.

3. In giving the background mesh size, the code assumes that the user knows this size is sufficiently small to be able to reasonably resolve the boundary curves. Choosing the background mesh h = 1 and the curves of size 0.1 cannot resolve a curve.

4. A linear search is used in "sizeFunctionMinimumOnBox" to find the minimum of the size function. A note is included in the subroutine that that should be switched to a good minimization algorithm.

5. Mesh sizes are determined (in part) by how close two curves are together (or how close a curve is to itself since the region they surround does not have to be convex). A bare minimum is three elements between since two will be deleted. The mesh size at any point along a curve is determined by a linear search through all points along the curve and all points along other curves. This is pretty bad, but meshing times so far have been su-second so there has been little incentive to get picky.

6. The sizer controls spread the region out using a Gaussian profile. The square root (for distance) and exponentials used are very expensive. Meshing times for meshes with sizer controls are significantly longer than those without. Since the actual shape of the regions is not important, consider using a quadratic polynomial approximation to a Gaussian instead to perhaps speed things up. In practice, of all the horrendous algorithms, the size controls seem to be the only ones that are costly.

7. Sizing due to topography is chosen according to its Gaussian curvature. The Gaussian curvature is approximated by second order finite differences, and is described in the document [approximation of Gaussian curvature](ApproximationOfGaussianCurvature.md).
