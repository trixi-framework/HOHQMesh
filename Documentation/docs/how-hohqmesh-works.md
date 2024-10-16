# How HOHQMesh Works

## Introduction
HOHQMesh is an automatic quad/hex mesh generator designed to be fully automatic. From the user's point of view, he/she creates a MODEL, which consists of (optionally) an outer boundary curve and any number of inner boundary curves. The user also includes CONTROL\_INPUT to direct which files are created, the file formats, and three aspects of the mesh creation process: the background grid, which is the basic mesh size desired, the smoother, and manual refinement by way of REFINEMENT\_CENTERs or REFINEMENT\_LINES. The mesher then uses the model curves and control parameters to automatically refine near model features to generate a mesh.

## Basic 2D Meshing strategy
The basic strategy for the 2D quadrilateral mesh generation is that devised by Robert Schneiders in "Algorithms for Quadrilateral and Hexahedral Mesh Generation". It appears to be unpublished but is available online.

The basic 2D steps are:

1. A uniform grid is laid down and a quadtree subdivision is performed until a size criterion is met. [MeshGeneratorMethods.f90/GenerateGridWithSizerAndType] 
2. The grid is made conforming by inserting templates for each possible non-conforming interface. There are 15 of these, plus rotations. [Templates.f90]
3. Elements are created from the quads [MeshGeneratorMethods.f90/GenerateNodesAndElements]
4. Exterior elements are marked in a "cookie-cutter" procedure and then removed. [MeshGeneratorMethods.f90/MarkExteriorElements] This gives a ragged boundary, which is smoothed.
5. Boundary edges are then normally projected onto the boundary curves [MeshGeneratorMethods.f90/LocateEdgeImagesOnBoundaries], thereby creating boundary elements.  [MeshGeneratorMethods.f90/GenerateBoundaryElements]
6. A topology cleanup is performed: High valence node elements are combined (Currently only valence 7) [MeshCleaner.f90/PerformTopologyCleanup/ReduceNodeValences] and diamond elements are collapsed. [MeshCleaner.f90/PerformTopologyCleanup/RemoveDiamondElements]. Many more topology cleanup methods have been proposed in the literature and could be added at this stage.
7. The mesh is smoothed using a Smoother. [MeshSmoother.f90] contains the base class for a smoother. The LaplaceSmoother subclass doesn't really work yet. The workhorse is the spring-dashpot in [SpringMeshSmoother.f90].
8. A final clean-up is performed [MeshCleaner.f90/PerformFinalMeshCleanup]. Bad elements are marked using a set of mesh quality analysis measures from the Verdict library: *C. Simpson, C. D. Ernst, P. Knupp, P. P. P ́ebay, and D. C. Thompson. The Verdict Library Reference Manual. Sandia National Laboratories, April 2007.URL www.vtk.org/Wiki/images/6/6b/VerdictManual-revA.pdf*.  Info about them is written to the Stats file. Chevron elements are removed with a template.
9. A final smoothing is done
10. Plot (for quick viewing) and mesh files are written.

## 3D Mesh Generation
Hex elements are created by extrusion of a 2D mesh. Three extrusions are possible:

- Simple Sweep: Sweep along a line in one of the three coordinate directions. [3DMeshController.f90/PerformSimpleMeshSweep]
- Simple Rotation: Rotate the 2D mesh through an angle about an axis. 
- Sweep along a given curve. [SweeperClass.f90] Sweep can simply follow the curve [SweeperClass.f90/applyDefaultSweepTransform], which is fully accurate but can introduce twisting in the 3D mesh, or use a parallel transport algorithm [SweeperClass.f90/applyHansonSweepTransform] which eliminates the twist, but is only 2nd order accurate in following the sweep curve.
