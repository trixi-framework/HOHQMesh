# The HOHQMesh Mesh File Formats

HOHQMesh currently can write in four mesh file formats, ISM, ISM-v2, ISM-MM, and Abaqus.
The ISM format was developed for the book "Implementing Spectral Methods: Algorithms for Scientists
and Engineers" by David A. Kopriva and it supplies all the information needed to define high order
curved elements of a spectral element mesh. Over time the format has evolved, with version ISM-v2
and ISM-MM added to include edge information and to handle multiple materials, respectively. The [Abaqus mesh file format](https://abaqus-docs.mit.edu/2017/English/SIMACAEMODRefMap/simamod-c-model.htm) is common in the finite element community.

## ISM

The ISM mesh file format can define either Quad or Hex elements. Quad elements are defined as in ISM and shown in Fig. 24 below.

![QuadElement](https://user-images.githubusercontent.com/25242486/195528246-aebbff54-8654-4aa9-8327-3259a11f2957.png)
<p align = "center"> Fig. 24. The Quad element definition with corner nodes (circles) and sides (squares)  in their standard ordering.</p>

Hex elements are defined in a standard finite element topology.

![HexElement](https://user-images.githubusercontent.com/25242486/195528290-fe4ce34a-f1ee-4c8e-855d-4a93f88ec5c7.png)
<p align = "center"> Fig. 25. The Hex element definition with corner nodes (circles) and faces (squares), in their standard ordering.</p>



The ISM format includes (corner) nodes and element connectivity, with additional edge interpolation data to define high order boundary information. It can define either 2D Quad elements or 3D Hex elements; however, at the present time, there is no header to define the element type. Rather, that information is inferred from the number of nodes that define the corners.

The top level view of the ISM file format is

	Header
	List of Nodes
	List of Elements

The header consists of one line if the format is ISM, two lines for the others. The first line for all but the ISM format include the mesh format type. The other line of the header specifies the size of the mesh and the order of the polynomial that is used to define boundary curves.

	#nodes  #elements  polynomialOrder

The list of nodes includes the (x,y,z) locations of the #nodes nodes in an ordered list

	x1 y1 z1
	x2 y2 z2
	.
	.
	.
	xN yN zN

The list of elements is an ordered list of element blocks,

	elementBlock1
	elementBlock2
	.
	.
	.
	elementBlockN

Each element block includes enough information to define a spectral element. Each block has

	node IDs of the four/eight corners [+ material name, if ISM-MM]
	Boundary flags for the sides/faces (0 = straight/flat, 1 = curved)
	Interpolation values for the curved sides
	Boundary names for sides/faces

The node IDs are the IDs (as determined by their location in the node list) of the nodes defined in the Node block. Additional values used for Hex elements are shown in square brackets.

	node1 node2 node3 node4 [node5 node6 node7 node8] [materialName]

The boundary flags are defined similarly,

	bf1 bf2 bf3 bf4 [b5 b6]

where the boundary numbering is defined in Figs. 24 and 25 above.

For each element boundary curve (or boundary face) for which the boundary flag = 1, a list of nodal values (x,y,z) is specified in order. The knots are assumed defined at the reversed Chebyshev Gauss-Lobatto points, $t_j = -\cos(j \pi /N)$. For Quad elements the boundary curve blocks are the nodal values

	x1,y1,z1
	x2,y2,z2
	.
	.
	.
	xN,yN,zN

For Hex elements, each block defines a surface patch, see below.

The last line of the element block lists names of the physical boundaries associated with a side/face.

	name1 name2 name3 name4 [name5 name6]

Interior (element-to-element) boundaries are denoted by --- (three dashes) to indicate no name.

### Algorithm

An algorithm for reading a quad mesh can therefore be written as

	Read nNodes nElements pOrder
	For n = 1 to nNodes
		Read x[n] y[n] z[n]
	End
	For n = 1 to nElements
		Read (nodeID[k,n], k = 1,...,4), [materialName]
		Read (bFlag[k,n], k = 1,...,4)
		For k = 1 to 4
			If bFlag(k) = 1 then
				For i = 0 to pOrder
					Read x[i,k,n] y[i,k,n] z[i,k,n]
				End
			End
		End
		Read (bName[k,n], k = 1,...,4)
	End
In this case, the index $i$ corresponds to the first coordinate direction and $j$ the second along the face.

The Hex element block is similar, except that the faces are defined with nodes at the two-dimensional tensor-product of the Chebyshev Gauss-Lobatto points.

	Read nNodes nElements pOrder
	For n = 1 to nNodes
		Read x[n] y[n] z[n]
	End
	For n = 1 to nElements
		Read (nodeID[k,n], k = 1,...,8) [,materialName]
		Read (bFlag[k,n], k = 1,...,6)
		For k = 1 to 6
			If bFlag(k) = 1 then
				For j = 0 to pOrder
					For i = 0 to pOrder
					   Read x[i,j,k,n] y[i,j,k,n] z[i,j,k,n]
					End
				End
			End
		End
		Read (bName[k,n], k = 1,...,6)
	End

If there are still questions, the source code for writing the mesh files can be found in `WriteISMMeshFile` in the file `MeshOutputMethods.f90` and `WriteISMHexMeshFile` in the file `Mesh3DOutputMethods.f90`.

### Example
As a concrete example, we present the mesh file for a circular domain with five elements, shown in Fig. 8.15 of the book "Implementing Spectral Methods", reproduced below.

![HexElement](https://user-images.githubusercontent.com/25242486/195528317-bb536e54-66df-4f9a-9417-819a09a25bc6.png)
<p align = "center"> Fig. 26. The Quad mesh for a circle for whose mesh file is shown below.</p>

The mesh has five elements with eight corner nodes. The outer boundary (called "outer") is eighth order, so it has nine points defined for each curve.

	 8 5 8 								       <- #Nodes #Elements Polynomial Order
	0.7000000000000000 -0.7000000000000000 0.0 <- Node 1 corner node location
	1.4142135623730951 -1.4142135623730949 0.0
	0.7000000000000000 0.7000000000000000 0.0
	1.4142135623730951 1.4142135623730949 0.0
	-0.7000000000000000 0.7000000000000000 0.0
	-1.4142135623730949 1.4142135623730951 0.0
	-1.4142135623730954 -1.4142135623730949 0.0
	-0.7000000000000000 -0.7000000000000000 0.0 <- Node 8 corner node location
	1 2 4 3									    <- First element block, node IDs
	0 1 0 0									    <- Side 2 is curved, others are straight
	1.4142135623730951 -1.4142135623730949 0.0  <- Start of interpolant nodes for side 2
	1.4961851763911174 -1.3271887273283636 0.0
	1.6994209839390670 -1.0544990845645972 0.0
	1.9103423681217324 -0.5921081291107658 0.0
	2.0000000000000000 0.0000000000000000 0.0
	1.9103423681217324 0.5921081291107657 0.0
	1.6994209839390670 1.0544990845645972 0.0
	1.4961851763911171 1.3271887273283638 0.0
	1.4142135623730951 1.4142135623730949 0.0 <- End of interpolation nodes for side 2
	--- outer --- ---					      <- Side 2 is named outer, others are interior sides
	5 3 4 6 							      <- Start of Element 2 block
	0 0 1 0
	-1.4142135623730949 1.4142135623730951 0.0
	-1.3271887273283636 1.4961851763911174 0.0
	-1.0544990845645970 1.6994209839390670 0.0
	-0.5921081291107656 1.9103423681217324 0.0
	0.0000000000000000 2.0000000000000000 0.0
	0.5921081291107658 1.9103423681217324 0.0
	1.0544990845645974 1.6994209839390668 0.0
	1.3271887273283638 1.4961851763911169 0.0
	1.4142135623730951 1.4142135623730949 0.0
	--- outer --- ---
	7 8 5 6
	0 0 0 1
	-1.4142135623730954 -1.4142135623730949 0.0
	-1.4961851763911174 -1.3271887273283636 0.0
	-1.6994209839390670 -1.0544990845645970 0.0
	-1.9103423681217326 -0.5921081291107655 0.0
	-2.0000000000000000 0.0000000000000000 0.0
	-1.9103423681217324 0.5921081291107659 0.0
	-1.6994209839390668 1.0544990845645974 0.0
	-1.4961851763911169 1.3271887273283638 0.0
	-1.4142135623730949 1.4142135623730951 0.0
	--- --- --- outer
	7 2 1 8
	1 0 0 0
	-1.4142135623730954 -1.4142135623730949 0.0
	-1.3271887273283640 -1.4961851763911169 0.0
	-1.0544990845645983 -1.6994209839390662 0.0
	-0.5921081291107669 -1.9103423681217322 0.0
	0.0000000000000000 -2.0000000000000000 0.0
	0.5921081291107663 -1.9103423681217322 0.0
	1.0544990845645961 -1.6994209839390677 0.0
	1.3271887273283627 -1.4961851763911180 0.0
	1.4142135623730949 -1.4142135623730954 0.0
	8 1 3 5
	0 0 0 0									<- Interior box, no curve values follow
	--- --- --- ---

## Additions for ISM-v2 and ISM-MM<a name="ISMv-2"></a>
The ISM-v2 and ISM-MM formats adds edge information to the ISM mesh file.

The first line of the mesh file will state that fact, that is, if the first line is ISM-V2 then it will have the edge information.

Line 1:

 	ISM-V2

The second line now also includes the number of edges in the mesh as follows:

	#nodes, #edges, #elements, polynomial order of boundary edges

The edges are read immediately after the nodes. For each edge the following are listed:

	start node ID, end node ID, element ID on left, element ID on right, side of left element, side of right element

These are the quantities that are computed in Alg. 148 of "Implementing Spectral Methods". If the edge is a boundary edge, then the second side element will be ID = 0 and the side of that element will be 0. If the sides have indices that increase in opposite directions, then the last column in the data will be negative.

## ABAQUS mesh file format<a name="ABAQUS"></a>

The [Abaqus mesh file format](https://abaqus-docs.mit.edu/2017/English/SIMACAEMODRefMap/simamod-c-model.htm) is common in the finite element community. The ABAQUS mesh file should use the `.inp` extension. The standard Abaqus format includes a list of node data and a list of element connectivity information. In this sense it is similar to the ISM format without high-order boundary information. The standard Abaqus format encodes a straight sided (linear) mesh.

Herein, we decribe an extended version to the ABAQUS format, divided into two parts. The first parts includes the standard Abaqus node and element lists. The second part encodes the high-order boundary information and naming information created by HOHQMesh. This second set of information is output in such a way that it will be ignored by standard ABAQUS file parsers, but the information is available if desired. For example, a `.inp` mesh file created by HOHQMesh can be parsed into [Trixi.jl](https://github.com/trixi-framework/Trixi.jl) to create high-order curved AMR meshes.

First, we describe the linear mesh skeleton encoded in the standard Abaqus format. For this example we use a quadrilateral mesh with 122 corner nodes and 103 elements:

	*Heading
 	 File created by HOHQMesh
	*NODE
	1, x1, y1, z1
	2, x2, y2, z2
	.
	.
	.
	122, x122, y122, z122
	*ELEMENT, type=CPS4, ELSET=Surface1
	1, 1, 2, 9, 8
	2, 2, 3, 10, 9
	.
	.
	.
	103, 7, 122, 87, 8

Note that the first column in the node or element list is used for indexing purposes. The four other indices in each line of the `*ELEMENT` list
are the corner IDs that dictate the element connectivity. These corner IDs are listed to guarantee right-handed element just as with the ISM format.
The Abaqus element type `CPS4` corresponds to a quadrilateral element. For the three-dimensional variant of this mesh file output we use

	*ELEMENT, type=C3D8, ELSET=Volume1

where `C3D8` corresponds to a hexahedron.

The standard Abaqus file format can be used to create a straight-sided mesh. The high-order boundary information and curvature
generated by HOHQMesh is output in the second portion of this mesh file. We demarcate between the two portions of the mesh file with the comment
line

	** ***** HOHQMesh boundary information ***** **

In the Abaqus format anything prefaced with `** ` is treated as a comment and is ignored by an ABAQUS file parser. Therefore, all the HOHQMesh
information output in the second portion of the mesh file is prefaced with `** ` to act as an Abaqus comment. After the above comment line the mesh
file gives the polynomial degree of the boundary curves in the mesh. Then, this mesh file format closely resembles the ISM format albeit slightly reordered. First, there is a list of the element connectivity, curved edge checks, and polynomial interpolant information after which comes the list of boundary names.

The order of the boundary names is a final difference in the mesh format. The Abaqus format and libraries that use it (e.g. [`p4est`](https://github.com/cburstedde/p4est)) require the boundary labels in a particular order. In general, we can think of a quadrilateral element to have sides labeled

                          +y
                   -----------------
                   |               |
                   | ^ y           |
                -x | |             | +x
                   | |             |
                   | ---> x        |
                   -----------------
                           -y

For this mesh file output the boundary labels are reordered to adopt the order convention of `-x +x -y +y`. For comparison, the default HOHQMesh
ordering used by ISM or ISM-v2 gives the boundary labels in the order `-y +x +y -x`. Similarly, the boundary names are reordered for the 3D HOHQMesh output to adopt the convention `-x +x -y +y -z +z` compared to the ISM formatting of 3D boundary names which is `-y +y -z +x +z -x`.