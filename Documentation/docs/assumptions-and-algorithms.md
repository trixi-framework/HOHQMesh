# Assumptions and Algorithms

This is a collection of comments on some of the approximations and assumptions made in developing HOHQMesh. It is not exhaustive. It is also not in any particular order.

1. When reading in the model, the subroutine [CheckForBoundaryIntersections](https://github.com/trixi-framework/HOHQMesh/blob/main/Source/Mesh/MeshBoundaryMethods.f90) tests the segments of the boundary curves for crossings, and uses a winding number to check that inner curves lie inside the outer boundary and not inside each other. The curves are tested as their point approximations, so a crossing between those points can be missed if the curves are poorly resolved.

2. The algorithm for parallel transport along a curve to sweep three dimensional meshes is only second order accurate. A 4th order scheme is published, but not implemented yet.

3. In giving the background mesh size, the code assumes that the user knows this size is sufficiently small to be able to reasonably resolve the boundary curves. Choosing the background mesh h = 1 and the curves of size 0.1 cannot resolve a curve.

4. In "sizeFunctionMinimumOnBox", the minimum of the size function from the refinement controls and the bottom topography is found by sampling each box on an 11x11 grid of points rather than with a minimization algorithm. A note is included in the subroutine that this should be switched to a good minimization algorithm. The size from the boundary curves does not use this sampling (see item 5).

5. Mesh sizes are determined (in part) by how close two curves are together (or how close a curve is to itself since the region they surround does not have to be convex). A bare minimum is three elements between since two will be deleted. To find these distances, the points of each curve chain are stored in a binary tree of bounding boxes. A point only searches the boxes within reach for the nearest point whose normal faces it, and the size function on a box only visits the boxes that overlap it.

6. The sizer controls spread the region out using a Gaussian profile, and the square root (for distance) and exponentials used are expensive. To limit the cost, each control is only evaluated closer than the distance where its size reaches the background mesh size, so boxes far from all the controls skip them. Since the actual shape of the regions is not important, a quadratic polynomial approximation to a Gaussian could speed things up further.

7. Sizing due to topography is chosen according to its principal curvatures. Background details on principle curvature are available in [Section 3.4](https://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node30.html) of the online textbook "Shape Interrogation for Computer Aided Design and Manufacturing" by N. M. Patrikalakis, T. Maekawa, and W. Cho. The formulas to compute the principle curvatures for explicit surfaces
are found in [Section 3.5.1](https://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node32.html) of the same textbook. In HOHQMesh these formulas are approximated by second order finite differences.
