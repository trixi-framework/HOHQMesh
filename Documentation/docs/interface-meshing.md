# Interface Boundary Meshing

The goal of a mesh with `INTERFACE_BOUNDARIES` is to create an all quadrilateral, conforming
mesh that provides high-order boundary information of internal (possibly embedded) curves.
The corresponding mesh format [ISM-MM](TheISMMeshFileFormats.md#BaseMaterialDefinition) then
labels elements with a material name.
Such meshes are suitable for spectral element approximations of problems like wave
scattering and heat transfer, where aligning material discontinuities with curved element
boundaries preserves high-order accuracy.

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

Generally, this cookie cutter strategy does not work for an interface boundary, because both
sides must shares the boundary nodes in order for the mesh to stay conforming.
HOHQMesh has two strategies for an interface:

1. **[Paving](#interface-paving)** (default): a band of elements with the size of the `BACKGROUND_GRID` around the curve are removed. This band is then meshed again from the curve outward. The nodes on the curve are placed by the geometry of the curve, and both sides share them.
2. **[Moving grid nodes onto the curve](#fallback-moving-grid-nodes-onto-the-curve)** (fallback): a closed
   path of existing edges of the background grid near the curve is chosen, and the nodes along that path are
   moved **onto** the interface curve. The elements next to the curve are then split with templates so that
   they have reasonable shapes on both sides.

Each interface chain is paved if possible. If paving fails for a chain, or if paving is switched off
(`interfacePaving` in `ProgramGlobals.f90`), that chain is meshed with the fallback strategy. So a mesh can contain paved and fallback interfaces.
Both strategies start from the same steps: the sizing, the
marking of the elements near the curve, the choice of the interface path and the pinning of nodes to the
joints of the chain (steps 1 to 5 of the [fallback](#fallback-moving-grid-nodes-onto-the-curve)).
Paving takes over at the end of step 5, in place of the final move of the nodes to their placement on the curve (`MoveInterfaceNodesToImages`) together with templates.
The elements around the path are then replaced, and only the nodes pinned to the joints are kept.

## Assumptions

- Every interface is a closed chain, oriented counter-clockwise, and does not cross any other
  boundary or interface curve. As for other boundaries, crossings are only roughly checked
  (see [Assumptions and Algorithms](assumptions-and-algorithms.md)).
- The background grid (possibly together with targeted `REFINEMENT_REGIONS`) is fine enough to resolve the curve.
  For example, small features, like a thin tip or a narrow gap between two curves, must span a few elements. The sizer uses the distance across the inside and the outside of an interface to make sure of this.
  A curve segment that is smaller than the local element size cannot be resolved, which can lead to poor meshes or failure.
  For the template (fallback) methodology this is especially important along the outer and inner boundaries, which are not paved.
  Paving sizes the elements along an interface from the curve itself and its local behavior (see below).
- With the fallback strategy, acute corners of an interface (below about 75°, i.e. a reflex angle above about 285° on the other side)
  are not handled well. The elements near the tip or such corners in the domain can be poorly shaped or folded, and folds there are not
  always caught by the Jacobian and area checks in the statistics, so check such meshes visually.
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

    An interface curve has elements on both sides, so the angle is $\alpha$ on one side
    and $360^\circ - \alpha$ on the other.
    The joint type only describes one side, so the interface code decides from the actual
    angle between the edges where it matters (e.g. for the corner constraints in the smoother).
- Corners of an interface are expected at the joints of its chain. Paving always puts a node at each
  joint, but a sharp bend inside a single curve (e.g. a spline through a corner) is only treated as
  strong curvature, which gives small elements there, not as a corner.

## Interface paving

Paving, or advancing front meshing, is an approach where the
elements are built from the boundary inward, one row at a time.
In HOHQMesh it is used locally, only along interface curves,
in a band around each interface.
The rest of the domain keeps the elements of the
background grid and applies the algorithm of Schneiders that uses templates.
Compared with moving grid nodes onto the curve and applying templates, paving offers three advantages:

- The element size along the curve comes from its geometry, i.e. curvature,
  its length between joints, and its distance to itself and to other curves, rather than from the
  background grid.
- Every joint of the chain gets a node, so the corners of the interface are kept sharp.
- The first row of elements on each side of an interface is aligned with the respective curve
  from the start, so no elements need to be split afterwards.

The routines are in `MeshGeneratorMethods.f90`. The main steps for one interface chain are:

| Step | What is done | Main routines |
|------|--------------|---------------|
| 1 | Sample the curve and find the element sizes along it | `SampleInterfaceChain`, `InterfaceCurveSizes` |
| 2 | Choose the band of elements to remove, keeping elements on the edge of the mesh and making the band edge one simple loop on each side of the curve | `PaveInterfaceWithBand` |
| 3 | Place the nodes on the curve | `PaveInterfaceWithBand` |
| 4 | Pave one row of elements on each side of the curve | `PaveRow` |
| 5 | Fill the gap between each new row and the edge of the band | `FillBetweenLoops`, `SplitFill`, `RemoveDoublets` |
| 6 | Try three band widths (of increasing size), apply the best result | `PaveInterface`, `worstQuadQuality`, `CommitPavePlan` |

Steps 2 through 5 work on a copy of the local geometry. They compute the list of elements to
remove, the positions of the new nodes on the curve and in the band, the new quads, and the quality
of the worst one, but they do not change the mesh. This result, the *plan*, is applied in step 6
only for the best band width.
After this choice, the band elements are removed, the new nodes are created, and the
new quads are added.

The plan for a particular band width is discarded, and the next width is tried, if

- the edge of the band does not form exactly one loop on the outside AND at most one loop on the inside of the curve (step 2)
- the edge of the band forms more than 32 loops (step 2)
- there would be fewer than four nodes on the curve (step 3)
- no valid row of elements can be paved along the curve (step 4)
- the gap between the new row and the edge of the band cannot be filled with quads (step 5)
- the band is one of the two wider ones and it contains a node of another curve (step 6)

If the plans for all three widths are discarded, the mesh is unchanged and the chain is meshed with
the fallback strategy.

### 1. Sizes along the curve

Each chain is sampled at equal steps of its parameter, 2000 samples per curve, or fewer for
a chain of many curves so that the total number of samples stays near 40000 (at least 200 per curve).
`InterfaceCurveSizes` then sets the element size $h$ at each sample to be the smallest of:

- the **control size** of the sizer (the base size and the refinement regions), limited so that
  at least `minNumberOfElementsInsideArea` elements fit across the extent of the curve,
- the **curvature size**, at which an element spans at most `paveMaxTurn` (30°) of turning of the
  curve. The turning is summed over a window one size long, which is found iteratively, and it is
  not summed across a joint, so a corner of the chain does not make the elements next to it small,
- the **proximity size**, the distance divided by `closeCurveFactor` to a part of the same curve that
  is near in space but at least three times as far along the curve, with no joint in between (e.g.
  across a thin tip or a narrow gap), or to a node of another curve that is still in the mesh.

The sizes are then graded so that they grow by at most half the distance along the curve. The
sizes used for the band (`hBand`) end here. The sizes used to place the nodes on the curve (`hs`)
are, in addition, at most the length of the piece of the chain between two joints, so that every
curve of the chain gets at least one edge, and graded again.

### 2. The band

All elements with a node closer to the curve than `paveBandWidth` (1.5) times `hBand` at the nearest
point of the curve are removed, together with all elements that touch a node of the interface path.
The band is then made "simple", so that its edge forms clean loops:

- An element with three or more sides on the band is added to it.
- At a node where the edge of the band touches itself, all elements around the node are added to the band.
- Elements with a side on the edge of the mesh, i.e. at an outer or inner boundary, are kept. A band
  can therefore reach the boundary, but it does not remove the elements there, and the boundary
  elements are generated later as usual.

The edge of the band **must** then be one loop outside of the curve and at most one loop inside of it.
No loop inside means that the entire inside of the curve is in the band. If there are more loops, the
band has cut the elements on one side of the curve into separate islands, e.g. where the curve passes
close to a boundary. The largest group of connected elements on each side of the curve is kept. The
other islands are handled by `ReconnectIsland`:

- An island of 20 elements or more is reconnected to the largest group if possible. The shortest
  path of band elements between them is put back, using only elements whose nodes are at least one
  local element size from the curve, or else at least half of one. The path may also cross smaller
  islands. Parts of the band that are then no longer attached to the curve are put back as well.
- An island that is smaller, or cannot be reconnected, is added to the band and paved over. The fill
  step then meshes it at about the size along the curve, which can give far more elements than it had.
  The elements of the large islands paved over are counted for the plan, see
  [step 6](#6-band-widths-and-the-choice).

The band is then made "simple" again, where its loops are checked. This is done up to four times
for one band width, with reconnection only in the first two, before paving fails for this band width.

### 3. Nodes on the curve

A node is put at each joint of the chain. Between two joints, the number of edges is the
integral of $1/h$ along the curve, rounded, and the nodes are placed at equal steps of that integral,
so that the edges follow the sizes `hs`. The node pinned to a joint in
[step 5 of the fallback](#5-joints-pinning-nodes-and-deciding-ties) is reused at that joint, so the joint keeps its node type. The other nodes on the curve are new.

A region that is filled with quadrilaterals must have an even number of edges on its boundary. The
loop of the band edge inside the curve is the edge of a region of quadrilaterals, so it always has an even number of edges.
The loop outside of the curve need not be, since the region outside of it can also be
bounded by other curves. The number of nodes on the curve is therefore chosen with the same parity as
the outside loop, by adding one edge to the piece whose rounding lost the most. Both sides share the
nodes on the curve.

### 4. First row of paved elements

`PaveRow` adds one row of elements on each side of the curve, along the polygon of the curve nodes.
A new node is placed off the curve, along the bisector of the angle $\theta$ at each curve node.
Its distance from the curve node is normally 0.8 times the mean length of the two curve edges at
that node. In a thin part of the region, the row could run into the other side. So, the distance is
also limited to 0.45 times the distance to the nearest other node of the curve or of the band edge.
How many elements a curve node gets depends on $\theta$, the angle on the side being paved:

| Angle at the curve node | Elements at the node | Purpose |
|---|---|---|
| $\theta <$ `paveWedgeAngle` (120°) | one, a "wedge" | the node is a corner of a single element, which closes the row at an acute corner |
| $\theta >$ `paveFanAngle` (240°) | three, a "fan" | the reflex angle is split into three angles of about 90° |
| otherwise | two | a regular row |

At a wedge, the two neighbors of the corner node do not get nodes of their own.
The wedge element is completed by the node opposite to the corner, at the fourth corner of a parallelogram.
Two wedges are not put next to each other.

The new row is rejected if one of its elements is folded, or if the new front crosses itself or the
band edge.
In that case the row is tried again with half the distance from the curve, down to a tenth of it, before paving fails for this band width.

### 5. Filling the gap

The region between the new front and the band edge is filled with quadrilaterals in `FillBetweenLoops`:

- If there is no band edge on this side (the whole inside of a curve is in the band), the front
  itself is the polygon to fill.
- Otherwise, two chords connect the front to the band edge, roughly opposite to each other, which cuts the
  region into two polygons. The chords are tried from the shortest pairs of nodes, and a chord must not
  cross either loop. New nodes are placed along each chord, about as far apart as the edges of the front, with the parity that gives both polygons an even number of nodes.

Each polygon is then split into quadrilaterals by `SplitFill`. It cuts the polygon recursively along
the chord that best splits the angles at its two ends, with a length close to a whole number of
edges, which gets new nodes along it. Nodes that already end a chord are penalized, which keeps the
valence low. A four-sided polygon is an element. Finally, `RemoveDoublets` merges the two elements at
a new node that has only two elements, since its angles would be near 180° in both and
smoothing cannot fix that.

### 6. Band widths and the choice

The best width of the band depends on the curve and its neighbors. A wider band leaves more room to
grade from the size along the curve to the size of the background grid, but it can run into other curves.
`PaveInterface`, thus, plans the paving for three widths: 1, 4/3 and 3/2 times `paveBandWidth`. A
wider band is rejected if it would remove an element with a node on another curve, since the paved
regions of two nearby curves would then meet in the gap between them and squeeze the elements there.

Each successful plan is scored by `worstQuadQuality` in three steps:

1. A copy of the plan's new interior nodes is smoothed with 20 Laplace passes, which is a cheap
   prediction of  how the smoother would move the nodes later.
2. The quality of each new element is computed. The smallest sine of the angles of each element times the ratio of
   its shortest to its longest side. Both flat and stretched elements therefore score low. The angles
   at the nodes of the band edge are left out, since the smoother moves those nodes later as well.
3. The score of the plan is the mean quality of the worst tenth of the new elements.

The plan that paves over the fewest elements of large islands (step 2) is applied, and among plans
that tie, the one with the highest score. `CommitPavePlan` applies the winning plan where
it removes the elements of the band and adds the new nodes and elements,
and the chain is marked as paved (`chainWasPaved`).

### After paving

The rest of the procedure remains the same as for the fallback, with two exceptions:

- The nodes on the curve are not redistributed along it (`RedistributeNodesAlongBoundaries`), since
  they are already placed by size and at the joints.
- The elements at the curve are not split (step 7 of the fallback), since the first row of elements is
  already aligned with the curve.

The topology cleanup, the smoothing with the corner constraints, the valence reduction on the interfaces, the untangling and the material assignment (steps 6 and 8 to 10 of the fallback) apply as usual.
During smoothing, the nodes on a paved curve slide along it and the nodes at the joints stay fixed, as for the fallback.
An edge between two nodes of a paved curve may not get shorter than 3/4 of its length at the start of each smoothing (`MIN_PAVED_EDGE_FRACTION` in `SpringMeshSmoother.f90`).
Otherwise the springs of squeezed elements, e.g. at the tip of a narrow wedge,
can pull a node along the curve almost onto its neighbor and leave a sliver there.

### Limitations

- Only interface curves are paved. The outer and inner boundaries use the cookie cutter together with Scheinders template approach. The element size along them comes from the background grid and the refinement regions. Where, for
  example, a short arc of the outer boundary meets a long straight line, the closely spaced nodes along the arc can meet coarse elements in a fan of thin elements. A refinement center at the arc helps there.
- Paving is not guaranteed to succeed. A chain for which all three band widths fail (see the
  [list of failure cases](#interface-paving)) is meshed with the fallback strategy. Its mesh then has
  the quality of the fallback, not of paving.
- The settings are in `ProgramGlobals.f90`, not in the control file:

    | Setting | Default | Meaning |
    |---|---|---|
    | `interfacePaving` | `.TRUE.` | pave the interfaces; otherwise always use the fallback |
    | `paveBandWidth` | 1.5 | width of the band on each side of the curve, in local element sizes |
    | `paveWedgeAngle` | 120° | below this angle, a curve node gets one element |
    | `paveFanAngle` | 240° | above this angle, a curve node gets three elements |
    | `paveMaxTurn` | 30° | the most turning of the curve along one element |
    | `paveSamplesPerCurve` | 0 | samples of each curve; 0 chooses them from the number of curves in the chain |

## Fallback: moving grid nodes onto the curve

This is the strategy for an interface chain that is not paved, either because paving failed for it or
because `interfacePaving` is switched off. Steps 1 to 5 also prepare the paving, see the
[overview](#overview). Instead of new elements, a closed path of existing edges of the background grid
mesh near the curve is chosen, and the nodes of that path are moved **onto** the interface curve. The
elements next to the curve are then split with templates so that they have reasonable shapes on both sides.

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

### 1. Sizing

The sizer refines the background grid so that there are sufficient elements between curves that face
each other and across thin parts of an interface curve. As an interface is meshed on both sides, the
distance from an interface curve to itself counts interior as well as across its exterior, depending
on the concavity of the curve (`Sizer.f90`).
The distances are found by searches over the points of the segmented curves, which
are copied into plain arrays and filtered with bounding boxes to keep large models (with many curve
segments or with many points) computationally efficient.

### 2. Marking interface elements

`MarkInterfaceElements` (`MeshGeneratorMethods.f90`) loops over the interface curves and the
elements of the background grid mesh. The four corners of each element are classified with the
winding number of the segmented curve, as `INSIDE` ($|w| > 0.6$) or `OUTSIDE` ($w = 0$). An element
with corners on both sides straddles the curve. An element whose corners are all on one side is
still marked if a point of the curve, or a point interpolated along a curve segment, lies inside
the element. The nodes of a marked element get the chain ID of the curve (`bCurveChainID`).

### 3. Choosing the interface path

The edges that will be moved onto the interface curve are chosen in
`CollectInterfaceEdgesFromElementSides` (`MeshBoundaryMethods.f90`). The idea is to classify
whole elements, not nodes, as inside or outside of the curve. The interface path is then the set
of edges between an inside and an outside element:

1. Each element is classified by its centroid. If the centroid is within a quarter of the
   element size of the curve, the fraction of the element's area inside the curve decides
   (inside if more than one half, `elementInsideFraction`).

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
   (`flipCornerWrappingElements`, `FindCornerTowardPoint`). The switch is skipped if a
   neighboring node already takes the corner.

5. A node with more than two interface edges is a "pinch". The least certain element touching
   it, i.e. the one with an inside fraction closest to one half, is switched. One pinch is
   removed at a time.

Steps 2 to 5 are repeated until no pinch elements remain, at most 100 times. If pinches remain, a
warning is posted. Finally, `PruneInterfaceEdges` removes dangling
edges and chords that are left at thin features, so that every node of the path has exactly two
interface edges. The nodes of the path are marked `INSIDE`, which here means "on the interface",
and their edges get the type `ON_INTERFACE`.

### 4. Moving the nodes onto the curve

Unlike for outer and inner boundaries, no boundary elements are generated for an interface.
The nodes of the path are associated with the nearest location on the interface curve
(`LocateEdgeImagesOnBoundaries`, `AssociateBoundaryEdgesToCurve`) and moved there
(`MoveInterfaceNodesToBoundary`). The nearest location on an interface curve is found by sampling the
curve parameter in a window around the previous guess before refining with `fmin`
(`ParametrizationAtPointNear`). This avoids finding a point on the wrong side of a thin
region of the domain, where the distance has more than one local minimum.

### 5. Joints: pinning nodes and deciding ties

After the nodes are on the interface curve, each joint of the chain gets one node pinned to it in `FlagEndNodes` (`MeshGeneratorMethods.f90`).
This is what keeps corners of the interface sharp.

- **Interior joints** ($t = k/n$, $k = 1,\dots,n-1$): the node whose chain parameter
  `gWhereOnBoundary` is closest to $k/n$ is pinned. The comparison is a strict over the nodes
  in the order of the boundary path, so if two nodes are equally close, the first one in the
  path wins. The distance is measured in the chain parameter, not in arc length.
- **The start/end joint** ($t = 0 \equiv 1$): the node with the smallest parameter $t_0$ and the
  node with the largest parameter $t_1$ are compared. If $t_0 < 1 - t_1$ the first one is
  pinned to $t = 0$, otherwise (including a tie) the second one is pinned to $t = 1$.
- **Row reversals**: all nodes within the minimization tolerance of the reversal point are
  pinned to it, since more than one node can be closest to it.

A pinned node gets `whereOnBoundary = 0` (or `1`) on its curve and the joint classification as its
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

### 6. Topology cleanup and the first smoothing

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
corner have been split (described next), and holding the new nodes on those lines would squeeze the
elements between them into slivers.

### 7. Templates for the elements at the interface

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

#### Corner elements

The three-way split cuts the angle at the middle interface node in two. That is fine where the
curve is smooth, but at a sharp corner of the interface it yields two thin elements.
If the middle interface node is pinned at a joint (its node type is not `ROW_SIDE`) and the element's
angle at that node is less than 135° (`MAX_CORNER_ANGLE`), the element is split into two instead
(`SplitCornerElementIntoTwo`). The two edges at the free node `f` are split at their midpoints
`P` and `M` as for the three-way split, so the neighbors still conform. The resulting hexagon
`f, P, f+1, f+2, f+3, M` is cut into two quads by one of the diagonals `P`–`(f+3)` or
`M`–`(f+1)`. The diagonal whose worse element has the larger minimum scaled Jacobian is chosen
(`quadMinScaledJacobian`); a tie goes to `P`–`(f+3)`. The corner node then belongs to one element
only and keeps its full angle.

### 8. Valences on the interface

Splitting an element with three nodes on the curve adds an element at its middle node. Where the
interface path turns, this leaves a fan of thin elements around a node of valence 7 on the curve.
After splitting, `ReduceNodeValences` is called again, but now only for nodes on interface curves.
The valence is reduced by `CleanUp7ValenceNode_InMesh`, which inserts a new off-curve node and
a diamond element, while the node itself stays on the curve. Nodes at or next to a joint are left
alone, since more elements on one side of a corner are expected there and these elements are
constrained by the smoother.

### 9. Untangling and opening flat elements

After the final smoothing, `UntangleInvertedElements` (`MeshCleaner.f90`) repairs the elements
that the smoother could not fix. It uses the **corner Jacobian** of an element: at each corner it
is the cross product of the two edges that meet there. It is positive at a convex corner of a
counter-clockwise element, zero where the corner is flat (180°) and negative where the element is
folded over at that corner. The two steps are:

1. **Untangle inverted elements.** An element where any corner Jacobian is zero or negative is
   folded (tangled), including one that is turned over completely, with all four corner Jacobians
   negative. The corner Jacobians that involve a given node are linear in the position of that
   node. Each of them can be written as the signed distance of the node from
   a line, so the positions where all corners around the node are positive form a convex polygon,
   the "kernel" of the node. Each node of a tangled element that is **not** on a boundary or interface
   curve is moved toward the point where the smallest of these distances is as large as possible,
   i.e. the point deepest inside the kernel (`MoveNodeIntoItsKernel`). The node moves only far
   enough to cover half of the possible improvement of the smallest distance, and it is kept within
   the box spanned by the surrounding elements, so that its neighbors are not disturbed too much.
   Moving one node changes the kernels of its neighbors, so this is repeated, up to 10 times, until
   no tangled elements remain. Nodes on curves are never moved, so an element whose nodes are all
   on curves cannot be untangled.

2. **Open up nearly flat elements.** An element with an angle above 165° is valid but nearly
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
    same kernel step as in step 1. In the example, `N` moves down into `C`, so that the angle of
    `C` at `N` drops below 180° and the angles of `A` and `B` at `N` open up. This is done once.

These steps apply to all elements, not only those at interfaces. These clean-up steps are needed
mostly where curves come close to each other or meet at a corner.

### 10. Material assignment

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
