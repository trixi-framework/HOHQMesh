# Interface Boundary Meshing

The goal of a mesh with `INTERFACE_BOUNDARIES` is to create an all quadrilateral, conforming
mesh that provides high-order boundary information of internal (possibly embedded) curves.
The corresponding mesh format [ISM-MM](TheISMMeshFileFormats.md#BaseMaterialDefinition) then
labels elements with a material name.
Such meshes can be used for spectral element approximations applied to, e.g., wave scattering
or heat transfer where high-order accuracy is retained because jump discontinuities in material
properties are aligned with (curved) boundaries.

## Overview

HOHQMesh has three types of boundary curves, each defined as a closed chain of curves:

- **Outer boundary**: all elements outside of this curve are removed.
- **Inner boundaries**: all elements inside any of these curves are removed, which creates holes.
- **Interface boundaries**: no elements are removed. Instead, the mesh is made to conform to the curve,
  so that element edges lie along it, and both sides of the curve are meshed. Interfaces are
  used to separate materials (see the [ISM-MM format](TheISMMeshFileFormats.md#BaseMaterialDefinition)),
  but they can also be used only to align the mesh with a curve.

Outer and inner boundaries are meshed with a "cookie cutter" approach. After elements of the background grid
are created, those that are outside of the domain or too close to a boundary are removed, and the ragged boundary
edges that are left are projected onto the curve to create a new layer of boundary elements.

Generally, this cookie cutter strategy does not work for an interface, because both sides must stay meshed and the two sides must share the nodes on the curve.
Instead, a closed path of existing edges of the background grid
mesh near the curve is chosen, and the nodes of that path are **moved onto** the interface curve.
The elements next to the curve are then split with templates so that they have reasonable
shapes on both sides.

The main steps of the interface meshing procedure are given below in the order in which they are performed.

| Step | What is done | Main routines |
|------|--------------|---------------|
| 1 | Size the background grid for both sides of the interface | `Sizer.f90` |
| 2 | Find the elements near each interface curve | `MarkInterfaceElements` |
| 3 | Choose the closed path of edges that becomes the interface | `CollectInterfaceEdgesFromElementSides`, `PruneInterfaceEdges` |
| 4 | Move the nodes of the path onto the curve | `LocateEdgeImagesOnBoundaries`, `MoveInterfaceNodesToBoundary` |
| 5 | Pin nodes to the joints of the chain and spread the others out | `FlagEndNodes`, `SpreadCollapsedBoundaryLocations`, `MoveInterfaceNodesToImages` |
| 6 | Topology cleanup and the first smoothing, with corner constraints | `PerformTopologyCleanup`, `SetInterfaceCornerConstraints` |
| 7 | Split the elements touching the interface with templates | `SplitInterfaceElements` |
| 8 | Reduce high valences on the interface | `ReduceNodeValences(nodesOnInterfaces = .TRUE.)` |
| 9 | Final smoothing, then untangle and open up nearly flat elements | `UntangleInvertedElements` |
| 10 | Assign the material IDs | `SetMaterialProperties` |

## Assumptions

- Every interface is a closed chain, oriented counter-clockwise, and does not cross any other
  boundary or interface curve. As for other boundaries, crossings are only roughly checked
  (see [Assumptions and Algorithms](assumptions-and-algorithms.md)).
- The background grid (possibly together with targeted `REFINEMENT_REGIONS`) is fine enough to resolve the curve.
  For example, small features, like a thin tip or a narrow gap between two curves, must span a few elements. The sizer uses the distance across the inside and the outside of an interface to make sure of this (see below).
  A curve segment that is smaller than the local element size cannot be resolved, which can lead to poor meshes and/or failure.
- Interfaces that are embedded in other interfaces are defined after the interfaces that contain
  them. Only the material assignment depends on this ("painter's algorithm").
- In the chain parameter $t\in[0,1]$, each of the $n$ curves of a chain gets an equal interval,
  so the joints are at $t = k/n$. The parameter is not proportional to arc length.
- At a joint, the chain is either smooth or has a corner. Corners are found from the angle
  between the curves at the joint (`JointClassification`), see the following table, with the
  interior angle $\alpha$ measured on the side of the elements:

  | Joint type | Interior angle |
  |---|---|
  | `ROW_END` | $\alpha \le 120^\circ$ |
  | `ROW_SIDE` (smooth) | $120^\circ < \alpha < 207^\circ$ |
  | `ROW_CORNER` | $207^\circ \le \alpha \le 300^\circ$ |
  | `ROW_REVERSAL` | $\alpha > 300^\circ$ |

  An interface has elements on both sides, so the angle is $\alpha$ on one side and
  $360^\circ - \alpha$ on the other. The joint type only describes one side, so the interface
  code decides from the actual angle between the edges where it matters (e.g. for the corner
  constraints in the smoother).

## 1. Sizing

The sizer refines the background grid so that there are sufficient elements between curves that face
each other and across thin parts of an interface curve. As an interface is meshed on both sides, the
distance from an interface curve to itself counts interior as well as across its exterior, depending
on the concavity of the curve (`Sizer.f90`).
The distances are found by searches over the points of the segmented curves, which
are copied into plain arrays and filtered with bounding boxes to keep large models (with many curve
segments or with many points) computationally efficient.

## 2. Marking interface elements

`MarkInterfaceElements` (`MeshGeneratorMethods.f90`) loops over the interface curves and the
elements of the background grid mesh. The four corners of each element are classified with the
winding number of the segmented curve, as `INSIDE` ($|w| > 0.6$) or `OUTSIDE` ($w = 0$). An element
with corners on both sides straddles the curve. An element whose corners are all on one side is
still marked if a point of the curve, or a point interpolated along a curve segment, lies inside
the element. The nodes of a marked element get the chain ID of the curve (`bCurveChainID`).

## 3. Choosing the interface path

The edges that will be moved onto the interface curve are chosen in
`CollectInterfaceEdgesFromElementSides` (`MeshBoundaryMethods.f90`). The idea is to classify
whole elements, not nodes, as inside or outside of the curve. The interface path is then the set
of edges between an inside and an outside element:

1. Each element is classified by its centroid. If the centroid is within a quarter of the
   element size of the curve, the fraction of the element's area inside the curve decides
   (inside if more than one half, `ElementInsideFraction`).

2. Only the largest connected inside region and the largest connected outside region are kept.
   All other regions are switched to the other side (`MergeDisconnectedRegions`), so that a single
   loop separates them.

3. An element with three or more interface edges is a one element wide bump or notch. For
   example, the "bump" inside element `B` below has outside neighbors on three of its sides, where the
   interface edges are marked by `‖` and `=`:

    ```text
       outside   outside   outside
      +--------+=========+--------+
      | outside‖    B    ‖ outside|
      +========+---------+========+
       inside    inside    inside
    ```

    The interface path would wrap around `B` along three of its edges, which touch all four of its
    nodes. All four nodes would then be moved onto a short, nearly straight piece of the curve,
    which flattens `B` into a sliver or folds it. Switching `B` to the other side straightens the
    path, so that only one of its edges lies on the interface:

    ```text
      +--------+---------+--------+
      | outside|    B    | outside|
      +========+=========+========+
       inside    inside    inside
    ```

    Now only two nodes of the "bump" element `B` are moved onto the curve and it keeps a regular quadrilateral
    shape. The interface curve passes close to `B`, so the cost is only that those two nodes move a
    little farther to reach it. A notch, i.e. an outside element that reaches one element deep
    into an inside region, is treated similarly.

4. An element whose two interface edges meet at a node would have three nodes on the curve:
   the shared node and the two nodes at the other ends of the edges. If the curve between the two end nodes has a
   corner that points toward the element, as at a reflex corner, the element would wrap around that corner
   and fold once its nodes are moved onto the curve. It is
   switched (once) so that the path runs through its fourth node, which then takes the corner
   (`FlipCornerWrappingElements`, `FindCornerTowardPoint`). The switch is skipped if a
   neighboring node already takes the corner.

5. A node with more than two interface edges is a "pinch". The least certain element touching
   it, i.e. the one with an inside fraction closest to one half, is switched. One pinch is
   removed at a time.

Steps 2 to 5 are repeated until no pinch elements remain, at most 100 times. If pinches remain, a
warning is posted. Finally, `PruneInterfaceEdges` removes dangling
edges and chords that are left at thin features, so that every node of the path has exactly two
interface edges. The nodes of the path are marked `INSIDE`, which here means "on the interface",
and their edges get the type `ON_INTERFACE`.

## 4. Moving the nodes onto the curve

Unlike for outer and inner boundaries, no boundary elements are generated for an interface.
The nodes of the path are associated with the nearest location on the interface curve
(`LocateEdgeImagesOnBoundaries`, `AssociateBoundaryEdgesToCurve`) and moved there
(`MoveInterfaceNodesToBoundary`). The nearest location on an interface curve is found by sampling the
curve parameter in a window around the previous guess before refining with `fmin`
(`ParametrizationAtPointNear`). This avoids finding a point on the wrong side of a thin
region of the domain, where the distance has more than one local minimum.

## 5. Joints: pinning nodes and deciding ties

After the nodes are on the interface curve, each joint of the chain gets one node pinned to it
(`FlagEndNodes`, `MeshGeneratorMethods.f90`). This is what keeps corners of the interface sharp.

- **Interior joints** ($t = k/n$, $k = 1,\dots,n-1$): the node whose chain parameter
  `gWhereOnBoundary` is closest to $k/n$ is pinned. The comparison is a strict over the nodes
  in the order of the boundary path, so if two nodes are equally close, the first one in the
  path wins. The distance is measured in the chain parameter, not in arc length.
- **The start/end joint** ($t = 0 \equiv 1$): the node with the smallest parameter $t_0$ and the
  node with the largest parameter $t_1$ are compared. If $t_0 < 1 - t_1$ the first one is
  pinned to $t = 0$, otherwise (including a tie) the second one is pinned to $t = 1$.
- **Row reversals**: all nodes within the minimization tolerance of the reversal point are
  pinned to it, since more than one node can be closest to it.

A pinned node gets `whereOnBoundary = 0` (or 1) on its curve and the joint classification as its
node type. There is no check that the same node is the closest node to two joints. If a curve of
the chain is shorter than the local element size, the later joint takes the node.

After this, the nodes in between are spread out (`SpreadCollapsedBoundaryLocations`). A node that is not
pinned is moved to the parametric midpoint of its neighbors provided its location is not ahead of its
neighbor along the curve, or if the distance to a neighbor on the curve is less than `0.4` times the
length of the original grid edge. `MoveInterfaceNodesToImages` puts the nodes at their new
locations.

Later, whenever the nodes are put back on the curve (`FindCurveLocationsforNodes`), a node that
is not pinned and runs into the end of its curve stays where it was instead of collapsing onto the
pinned node at the joint. Pinned nodes are kept fixed during smoothing (`SetNodeActiveStatus`).
All other interface nodes slide along the curve where the smoother removes the normal component of
their acceleration. This matches the strategy used for nodes on a physical boundary.

## 6. Topology cleanup and the first smoothing

The interior topology cleanup (`PerformTopologyCleanup`) reduces valence 7 nodes and removes
diamond elements away from the curves. The nodes on the boundary and interface curves are left
alone at this stage.

During the first smoothing, the elements at a sharp corner of an interface get extra help
(`SetInterfaceCornerConstraints`, `SpringMeshSmoother.f90`). The candidates are the interface
nodes pinned at a joint, and the angle between the two interface edges at the node decides
whether there is a corner:

- If the reflex side of the corner is shared by three elements, the two edges from the corner
  into that region should continue the two sides of the corner, so that all three elements have
  angles of about 90° at the corner. The springs only balance edge lengths and would pull these
  edges together. The nodes at the ends of those edges are therefore moved onto the extensions of
  the sides and constrained to move along them.
- If the reflex side is shared by two elements, the edge between them is put on the bisector of
  the reflex angle and constrained to it (e.g. 135° + 135° at a 90° corner).
- Constrained nodes are kept a minimum distance from the corner.

Corners where the mesh does not have one of these layouts are left alone. The constraints to the
side extensions only apply in the first smoothing. In the second smoothing the elements at the
corner have been split (next section), and holding the new nodes on those lines would squeeze the
elements between them into slivers.

## 7. Templates for the elements at the interface

After the nodes are on the interface curve, the elements on both sides of it can be triangles (three nodes
on the curve), wedges (one node on the curve) or regular quads (two nodes, one edge on the curve).
In the final cleanup, `SplitInterfaceElements` (`InterfaceElementMethods.f90`) splits every
element that has a node on an interface. The rule that keeps the mesh conforming is simple:

> **Every edge with exactly one node on the interface is split at its midpoint.**

Whether an edge is split only depends on the edge itself, so the two elements that share it
always agree. The midpoint node is stored on the edge (`auxiliaryNode`) so that both elements use
the same one. The templates are chosen by the number of nodes of the element on the interface
(`*` marks an interface node, `C` a new center node, `m` a new edge midpoint):

**Two adjacent nodes** (one edge on the interface): split into two elements, which halves the
element normal to the interface.

```
  4 ------- 3          4 ------- 3
  |         |          |         |
  |         |   -->    m ------- m
  |         |          |         |
  1*-------*2          1*-------*2
```

**One node**, or **three nodes**: split into three elements. The node opposite to the split
corner is shared by two of them. For one interface node the interface node is the split corner
`f`. For three interface nodes the free node is the split corner, and the middle interface node is
opposite to it.

```
  4 ------- 3          4 ------- 3
  |         |          |       / |
  |         |   -->    m --- C   |
  |         |          |     |   |
  f ------- 2          f --- m - 2
```

**Two opposite nodes**: all four edges are split, plus a center node, into four elements.

```
  4*------- 3          4*--- m --- 3
  |         |          |     |     |
  |         |   -->    m --- C --- m
  |         |          |     |     |
  1 -------*2          1 --- m ---*2
```

**Zero or four nodes**: the element is kept as is.

### Corner elements

The three-way split cuts the angle at the middle interface node in two. That is fine where the
curve is smooth, but at a sharp corner of the interface it leaves two thin elements in the corner.
If the middle interface node is pinned at a joint (its node type is not `ROW_SIDE`) and the element's
angle at that node is less than 135° (`MAX_CORNER_ANGLE`), the element is split into two instead
(`SplitCornerElementIntoTwo`). The two edges at the free node `f` are split at their midpoints
`P` and `M` as for the three-way split, so the neighbors still conform. The resulting hexagon
`f, P, f+1, f+2, f+3, M` is cut into two quads by one of the diagonals `P`–`(f+3)` or
`M`–`(f+1)`. The diagonal whose worse element has the larger minimum scaled Jacobian is chosen
(`QuadMinScaledJacobian`); a tie goes to `P`–`(f+3)`. The corner node then belongs to one element
only and keeps its full angle.

## 8. Valences on the interface

Splitting an element with three nodes on the curve adds an element at its middle node. Where the
interface path turns, this leaves a fan of thin elements around a node of valence 7 on the curve.
After splitting, `ReduceNodeValences` is called again, but now only for nodes on interface curves.
The valence is reduced by `CleanUp7ValenceNode_InMesh`, which inserts a new off-curve node and
a diamond element, while the node itself stays on the curve. Nodes at or next to a joint are left
alone, since more elements on one side of a corner are expected there and these elements are
constrained by the smoother.

## 9. Untangling and opening flat elements

After the final smoothing, `UntangleInvertedElements` (`MeshCleaner.f90`) repairs the elements
that the smoother could not fix. It uses the **corner Jacobian** of an element: at each corner it
is the cross product of the two edges that meet there. It is positive at a convex corner of a
counter-clockwise element, zero where the corner is flat (180°) and negative where the element is
folded over at that corner. The three steps are:

1. **Renumber clockwise elements.** An element whose four corner Jacobians are all negative is not
   folded. Its nodes are just numbered clockwise, e.g. in a patch of elements that was split from
   a folded element. The node order of such an element is reversed.

2. **Untangle inverted elements.** An element where some, but not all, corner Jacobians are zero
   or negative is folded (tangled). The corner Jacobians that involve a given node are linear in
   the position of that node. Each of them can be written as the signed distance of the node from
   a line, so the positions where all corners around the node are positive form a convex polygon,
   the "kernel" of the node. Each node of a tangled element that is **not** on a boundary or interface
   curve is moved toward the point where the smallest of these distances is as large as possible,
   i.e. the point deepest inside the kernel (`MoveNodeIntoItsKernel`). The node moves only far
   enough to cover half of the possible improvement of the smallest distance, and it is kept within
   the box spanned by the surrounding elements, so that its neighbors are not disturbed too much.
   Moving one node changes the kernels of its neighbors, so this is repeated, up to 10 times, until
   no tangled elements remain. Nodes on curves are never moved, so an element whose nodes are all
   on curves cannot be untangled.

3. **Open up nearly flat elements.** An element with an angle above 165° is valid but nearly
   degenerate. An element with a maximum angle of 180° it is really a triangle with an extra node on one side.
   The springs cannot always open such an angle, because they only balance edge lengths. A typical case
   is a node `N` of valence 3 where two of its three edges lie on a straight line:

    ```text
      +------+------+
      |  A   |  B   |
      +------N------+      N has three edges: left, right and up
      |      C      |
      |             |      C has an angle of 180° at N
      +-------------+
    ```

    The springs along the line pull equally in opposite directions, and nothing pushes `N` off the
    line into `C`, so `N` is in equilibrium although `C` is flat at `N`. For each element that is
    not tangled but has an angle above 165°, its nodes that are not on a curve are moved with the
    same kernel step as in step 2. In the example, `N` moves down into `C`, so that the angle of
    `C` at `N` drops below 180° and the angles of `A` and `B` at `N` open up. This is done once.

These steps apply to all elements, not only those at interfaces. These clean-up steps are needed
mostly where curves come close to each other or meet at a corner.

## 10. Material assignment

For the ISM-MM format, `SetMaterialProperties` assigns the material IDs after all changes to the
mesh are done. The IDs themselves are set up before meshing, from the `material name` of each
interface chain (or its name if there is none), see the
[ISM-MM](TheISMMeshFileFormats.md#BaseMaterialDefinition). For each interface curve in the order
of definition, an element is given the curve's material ID if more of its nodes are inside the curve
than outside. The nodes that were moved onto this curve are not counted, since a node on the curve
can be on either side of the polygon through the curve points. If the remaining nodes do not
decide, the centroid of the element decides. Since later curves overwrite earlier ones, an element
gets the material of the innermost curve that contains it, provided embedded curves are defined
after the curves containing them.

<!--
## Open questions and known limitations

- The order of embedded interface curves matters for the material assignment. It could be
  determined automatically by testing whether one curve lies inside another.
- Joints are matched to nodes by the chain parameter, not by arc length. For chains whose curves
  have very different lengths, the closest node in parameter may not be the closest one in space.
- A curve of a chain that is shorter than the local element size cannot get its own nodes, and
  two joints can compete for the same node.
- Several thresholds are empirical: the winding number threshold of 0.6, the 0.4 ratio for spreading
  nodes, the corner angle of 135° for the two-way corner split, and the 165° limit for opening flat
  elements.
-->
