# ChangeLog

## Changes in the v1.7 lifecycle

- Pave the outer and inner boundaries (#202). The elements along the `OUTER_BOUNDARY` and `INNER_BOUNDARIES` are
  paved from the curve, one-sided, at the size of the quadtree grid at the curve, before the interfaces are paved.
  Paving is ON by default and is switched off with `paving = OFF` in the `OUTER_BOUNDARY` or `INNER_BOUNDARIES`
  block. A boundary for which paving fails or is switched off is meshed by projection, as before.
- Improve interface paving (#201). A wedge that would fold the row is moved to the neighboring corner, the band
  keeps out elements with nodes on other curves, narrow strips and slots of a curve get elements that fit
  across, and the gap fill is spaced by the local edge lengths to avoid over-refined regions.

## Changes in the v1.6 lifecycle

- Fix and speed up boundary curve optimization (#196). A new bracketed root finder gives the same results on every
  platform and stops `connect` from running out of memory, and the requested `continuity` and `tolerance` are now
  applied to the curved boundaries written to the mesh.
- Implement paving strategy for the interior/exterior meshing (#190) with the existing strategy as a fallback.
  Paving is ON by default and is switched off with `paving = OFF` in the `INTERFACE_BOUNDARIES` block
- Increase test coverage to over 95% with many new unit and benchmark tests (#199), which uncovered and fixed several bugs:
  the parabolic step in Brent's method (`fMin`) was never taken, `SIMPLE_EXTRUSION` along x or y mishandled curved
  boundaries, and several invalid inputs crashed instead of giving an error. Also, the remesh retry now runs
  at most once and always reports it, the 2D `ROTATION_TRANSFORM` is documented, and file paths may be up to 512
  characters long.
- Remove unused `MESH_PARAMETERS` block (#197) as well as 3-Refinement templates and associated functions (#198)
- Add cleanup for high valence nodes left due to quadtree transition templates. A new shape improvement
  step after smoothing repositions the nodes of poorly shaped elements, and preserves the symmetry of
  symmetric meshes (#194)
- Improve the mesh cleanup: better diamond element removal and untangling of inverted elements
  no longer lets nodes drift from their original positions. The sizer is faster for models with
  many boundary points or refinement regions, and several memory leaks are fixed (#191)
- Add script to update Benchmark values and associated files (#195)
- Improve strategy for `INTERFACE_BOUNDARIES` and include documentation of interior/exterior meshing (#186).
- New strategy to `RedistributeNodesAlongBoundaries` added (#179). The number of times nodes along
  the boundary are smoothed is `numBoundarySmoothPasses = 10` set in the `ProgramGlobals.f90`
- Add boundary curve L2/H1 optimization and error controlled adaptive meshing capability (#166)
- Whether or not boundary curve optimization is requested, HOHQMesh reports the maximum $L^2$ and $H^1$ errors along the boundaries

## Changes in the v1.5 lifecycle

- Add functionality for an `EllipticArc` boundary curve (#112)
- Fixes a bug where material properties where not properly assigned when there was no outer boundary.
- Refinement for bottom topography uses the maximum principal curvature now.
- Documentation now explains that bottom topography refinement can be applied to two-dimensional meshes, too.
- A section on "What you can do with HOHQMesh" is added to the documentation.
- Information on how to cite the JOSS paper is added.

