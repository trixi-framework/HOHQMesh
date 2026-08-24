# ChangeLog

## Changes in the v1.6 lifecycle

- New strategy to `RedistributeNodesAlongBoundaries` added (#179). The number of times nodes along
  the boundary are smoothed is `numBoundarySmoothPasses = 10` set in the `ProgramGlobals.f90`.
- Add boundary curve L2/H1 optimization and error controlled adaptive meshing capability (#166).
- Whether or not boundary curve optimization is requested, HOHQMesh reports the maximum $L^2$ and $H^1$ errors along the boundaries.

## Changes in the v1.5 lifecycle

- Add functionality for an `EllipticArc` boundary curve (#112)
- Fixes a bug where material properties where not properly assigned when there was no outer boundary.
- Refinement for bottom topography uses the maximum principal curvature now.
- Documentation now explains that bottom topography refinement can be applied to two-dimensional meshes, too.
- A section on "What you can do with HOHQMesh" is added to the documentation.
- Information on how to cite the JOSS paper is added.

