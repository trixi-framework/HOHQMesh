
# TL;DR. What you can do with HOHQMesh<a name="WhatToDo"></a>

To use HOHQMesh to generate all-quadrilateral meshes with arbitrary order boundary elements you

- Define a [`MODEL`](the-model.md#TheModel) consisting of
	- An optional closed outer [boundary curve](the-model.md#Boundaries) made up of one or a chain of curved segments defined by primitives like straight [line segments](the-model.md#EndPointsLine), [circular arcs](the-model.md#CircularArc), [elliptic arcs](the-model.md#ParametricEqn), [splines](the-model.md#Spline), or [equations](the-model.md#ParametricEqn)
	- Zero or more closed inner boundary curves defined in the same way
	- Zero or more internal boundary curves that define boundaries for multiple material applications
	- An optional bottom [topography](three-dimensional-hexahedral-meshes.md#Topography) defined either in functional form or from a file to use to [refine](three-dimensional-hexahedral-meshes.md#SizingTopography) a 2D mesh around bottom features. (For example for shallow water equation computations.)

- Tell it how to mesh the model with a [`CONTROL_INPUT`](the-control-input.md) section to control the meshing process by
  - Setting [run parameters](the-control-input.md#RunParameters) that specify where to write the results, specify the mesh and plot file formats and the polynomial order of the boundary curves
  - Setting [background grid](the-control-input.md#BackgroundGrid) size to specify the largest element size desired
  - Setting how the mesh should be [smoothed](the-control-input.md#Smoother)
  - Defining optional [refinement regions](the-control-input.md#RefinementRegions) to allow manual refinement of the mesh to emphasize specific regions not indicated by the boundary curves or topography.

HOHQMesh will automatically generate a mesh with curved elements sized according to the geometry, like the curvature of the boundary curves and bottom topography, and the distance between boundary curves. It will generate a mesh that is symmetric about a symmetry line if segments of the outer boundary are defined as [symmetry boundaries](the-model.md#Symmetry).

Additionally, you can generate an all [hexahedral](three-dimensional-hexahedral-meshes) mesh by extruding a quadrilateral mesh by

- [Simple extrusion](three-dimensional-hexahedral-meshes.md#Extrusion) along a coordinate direction
- [Simple rotation](three-dimensional-hexahedral-meshes.md#Rotation) about an axis
- [Sweeping](three-dimensional-hexahedral-meshes.md#Sweeping) a quadrilateral mesh along a curve and optionally [scaling](three-dimensional-hexahedral-meshes.md#Scaling) the width along the way

In hexahedral mesh you can have the bottom of the hexahedral mesh follow a prescibed [topography](three-dimensional-hexahedral-meshes.md#Topography) defined in functional form or from data. The mesh can also be [sized](three-dimensional-hexahedral-meshes.md#SizingTopography) according to the curvature of the bottom topography.

# Introduction

Multidomain spectral methods, of which spectral element methods (SEMs) are a subclass, were introduced by Patera  (for elliptic and parabolic equations) and by Kopriva (for hyperbolic systems) to increase the efficiency of spectral methods and to apply them to complex geometries. Although somewhat controversial at the time -- questions were raised whether it was wise to not use the highest order polynomial possible  for a given number of degrees of freedom -- the methods have become so commonly used within the community that the updated book by Canuto et al.  is subtitled  “Fundamentals in Single Domains.”

The features of spectral element methods are now well-established. Like low order finite element methods, they can be applied to general geometries, but have exponential convergence in the polynomial order. Discontinuous Galerkin (DGSEM) versions applied to hyperbolic problems have exponentially low dissipation and dispersion errors, making them well suited for wave propagation problems. They are also especially suitable when material discontinuities are present. Approximations exist for high order quad/hex and tri/tet elements. Numerous examples of the flexibility and the power of spectral element methods can be found in Canuto et al.’s third volume subtitled “Evolution to Complex Geometries”. Textbooks on the subject now exist, such as those by Deville, Fischer and Mund, Sherwin and Karniadakis , Hesthaven and Warburton, and Kopriva.

What some are now calling “classical” spectral element methods use tensor product bases on quadrilateral or hexahedral meshes. These bases lead to very efficient implementations and have high order quadratures that can be used to approximate the integrals found in weak forms of the equations. The methods are being used in a wide variety of fields including fluid dynamics, electromagnetics, geophysics, and fluid-structure interaction problems, just to name a few.

Unfortunately, meshes for quad/hex elements are considered to be difficult to generate even for low order finite element approximations. This has lead to the development of triangular/tetrahedral spectral element bases. These methods can adapt the meshes generated by virtually all mesh generation packages today in two and three space dimensions. What one gives up in trade is the efficiency of the derivative evaluations, the Gauss quadratures, and meshes well-suited for boundary layer computations.

### Spectral Element Grid Generation<a name="SpectralElementGridGeneration"></a>

The advantages not withstanding, a major frustration in - and impediment to - the application of spectral element methods has been the lack of appropriate general purpose mesh generation software. A survey of the literature, practitioners, and user manuals for available spectral element software packages such as SemTex, SEM2DPack, or Nekton, highlights these difficulties. Blackburn's SemTex page [http://users.monash.edu.au/~bburn/semtex.html]() notes that “Mesh generation can be a significant hurdle to new users” and includes “a number of example meshes ... (most of which were generated by hand).” SEM2DPack's manual says it “can only generate a structured mesh for a single quadrilateral domain, possibly with curved sub-horizontal boundaries and curved sub-horizontal layer interfaces.” Sherwin and Peiro's comment:  “The ability to construct suitable computational meshes is currently a significant limiting factor in the development of compact high-order algorithms in very complex geometries” still holds today. Canuto et al. do not even broach the subject.

Simply put, and avoiding the common colloquialism, the state of the art in spectral element grid generation has been dismal.
One finds that spectral element meshes are either generated “by hand”, by special purpose mesh generators, or by low order finite element packages. Examples of hand generated meshes can be found in the textbooks listed above, for instance. SEM2DPack interfaces with the low order finite element mesh generator EMC2. The Nekton and SemTex packages interface with the finite element package GMSH. But the situation is particularly difficult for ``classical'' quad and hex element codes since even low order finite element mesh generators for these elements are hard to find. A consequence is that one even finds meshes in the literature that are simple quad/hex decompositions of low order triangular/tetrahedral meshes.
The meshes that practitioners generate differ greatly from those generated by finite element mesh generators. The reason is not just a matter of the tedium associated with the process. Spectral element approximations encourage the use of larger elements with curved boundaries approximated at high order. Meshes generated by hand or with simple templates tend to have fewer and larger elements.

Meshes generated by finite element packages designed for low order elements generate huge numbers of small elements and do not exploit the efficiency of high order spectral element approximations. The use of standard generators can lead one to use a high order method, yet approximate curved boundaries as segments of straight lines. Commercial mesh generators that generate “higher order elements”, e.g.  PATRAN, GMSH or Gambit, do exist, but high order usually means third order, tops. ICEM-HEXA will guarantee quad/hex spectral element type meshes only for block structured meshes. The costs of commercial packages, however, are so far above the budgets provided by the typical NSF grant or mathematics department and so aren’t an option even if they could generate spectral element meshes.

### HOHQMesh<a name="HOHQMesh"></a>
For these reasons we have developed the High Order Hex-Quad Mesh (HOHQMesh) package to automatically generate all-quadrilateral meshes with high order boundary information to be used in spectral element computations. It also can take such two dimensional meshes and extrude them in the normal direction to general all hex meshes for simple extrusion type geometries.

## Example Meshes<a name="ExampleMeshes"></a>

Before going into details, we show some meshes that have been generated by HOHQMesh. Control files for generating these meshes can be found in the Examples directory. Some of the meshes show internal spectral element degrees of freedom and the fully accurate boundary representations. Others show only the quad or hex shape of the elements as given in the plot file generated by the program.

The first example is a full spectral element mesh for three circles within an outer circle. HOHQMesh is fully automatic and sizes the elements according to the geometry.

![Circles3](https://user-images.githubusercontent.com/3637659/121807814-09e21f80-cc56-11eb-9e4d-fad5929c6822.png)
<p align = "center"> Fig. 1. Sixth order spectral element mesh of three circles within a circle (<em>Examples/2D/Circles3</em>)</p>

The second example example shows that general curves can be used to define the boundaries. This time, a set of points and a spline are use to define the outer boundary.
![SplineGeometry](https://user-images.githubusercontent.com/3637659/121807895-5e859a80-cc56-11eb-820a-73c4be8676ee.png)
<p align = "center"> Fig. 2. Eighth order spectral element mesh of a domain bounded by a spline curve (<em>Examples/2D/Spline</em>)</p>

HOHQMesh has templates to automatically mesh around sharp corners.
![CavityRamp](https://user-images.githubusercontent.com/3637659/121807804-fd5dc700-cc55-11eb-8cfe-2b21e79f0af7.png)
<p align = "center"> Fig. 3. Spectral element mesh of a domain with sharp corners (<em>Examples/2D/CavityRamp</em>)</p>

This makes it possible to mesh airfoil type geometries.
![NACA0012](https://user-images.githubusercontent.com/3637659/121807856-3b5aeb00-cc56-11eb-9b6f-5ec8d22da158.png)
<p align = "center"> Fig. 4. Mesh of a NACA0012 airfoil (<em>Examples/2D/NACA0012</em>)</p>

![KT3Element](https://user-images.githubusercontent.com/3637659/121807848-2da56580-cc56-11eb-9f0a-1a078cc3e612.png)
<p align = "center"> Fig. 5. Mesh for a three element Karman-Trefftz airfoil (<em>Examples/2D/KT3Element</em>)</p>

Local refinement can be added manually, either at a point or along a line.
![AllFeatures](https://github.com/user-attachments/assets/2f12ca1c-8a20-4c7d-98e6-9d619562a34d)
<p align = "center"> Fig. 6. Mesh showing manual refinement along a line and at a point (<em>Examples/2D/AllFeatures</em>)</p>

Truly complex geometries can be meshed, as shown in the following coastline models. Refinement around features is automatic.
![IndianOcean](https://user-images.githubusercontent.com/3637659/121807841-267e5780-cc56-11eb-9ef3-a1de263a8a81.png)
<p align = "center"> Fig. 7. The Indian Ocean (<em>Examples/2D/IndianOcean</em>)</p>

![Superior](https://user-images.githubusercontent.com/3637659/121807899-65141200-cc56-11eb-990e-4550feb0f90e.png)
<p align = "center"> Fig. 8. Lake Superior with spectral element nodes shown (<em>Examples/2D/LakeSuperior</em>)</p>

In some applications, such as if there are regions in which the material properties change abruptly, one may want to include interior interface curves to ensure that element boundaries fall along those curves. Fig. 9 shows a circular domain with two interior circular domains that are separated by interface curves.

![B&M](https://user-images.githubusercontent.com/25242486/241190479-ba50d797-3bd9-41b1-bc38-03fc86f8c3da.png)
<p align = "center"> Fig. 9. Mesh with interior interfaces bounded by circles.</p>


Finally, hex meshes can also be created by sweeping a quad mesh. The simplest way is to extrude a two-dimensional mesh in one of the coordinate directions.

![CavityRampExtruded](https://user-images.githubusercontent.com/3637659/121807810-051d6b80-cc56-11eb-8c89-7911bc152bfb.png)
<p align = "center"> Fig. 10. Simple extrusion of the mesh in Fig. 3 (<em>Examples/3D/CavityRampExtruded</em>)</p>

Or the mesh can be rotated along a coordinate axis through a specified angle,

![IglooAlt](https://user-images.githubusercontent.com/3637659/121807832-1e261c80-cc56-11eb-8c86-9d2a9e07de00.png)
<p align = "center"> Fig. 11. Simple rotation of the mesh in Fig. 3</p>

More sophisticated extrusions can be created by sweeping along a curve,

![Snake](https://user-images.githubusercontent.com/3637659/121807890-588fb980-cc56-11eb-9698-d3efffafed82.png)
<p align = "center"> Fig. 12. Hex mesh generated by sweeping along a curve (<em>Examples/3D/Snake</em>)</p>

Finally, swept meshes can also be scaled along the curve.
![ScaledSigmoid](https://user-images.githubusercontent.com/3637659/121807887-54639c00-cc56-11eb-9474-9d809320af9b.png)
<p align = "center"> Fig. 13. Hex mesh generated by sweeping and scaling along a curve</p>

All told, the geometries that can be meshed can be quite general.
![GingerbreadMan](https://user-images.githubusercontent.com/3637659/121807819-0fd80080-cc56-11eb-9adc-7b3b24095dcb.png)
<p align = "center"> Fig. 14. Quad mesh of a gingerbread man with spectral element nodes shown (<em>Examples/2D/GingerbreadMan</em>)</p>