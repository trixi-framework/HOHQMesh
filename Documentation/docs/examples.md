# Examples

HOHQMesh comes with 12 example control, mesh, and plot files for two-dimensional meshes found in the Examples/2D directory. These examples illustrate the use of the the control and model features, and the resulting meshes and plot files.

Also included in the Examples directory is a control template: ControlTemplate.control, 
that can (like any of the examples) be modified at will.

| Name        | Description                          | Outer | Inner | Chain | Parametric Eqn | Spline | Line | Arc | Refinement Center | Refinement Line |
| ----------- | ------------------------------------ |-------|-------|-------|----------------|--------|------|-----|--------------| --------------- |
| AllFeatures  | A triangle with three holes  |:material-check: | :material-check:3 | :material-check: | :material-check: | :material-check: | :material-check: | :material-check: | :material-check:| :material-check: 
| CavityRamp  | A cavity domain with a sloping ramp on the exit side  |:material-check: | :material-close: | :material-check: | :material-close: | :material-close: | :material-check: | :material-close: | :material-close:| :material-close:|
| Circles3  | Three circles enclosed by a large circle |:material-check: | :material-check:3 | :material-close: | :material-check: | :material-close: | :material-close: | :material-close: | :material-close:| :material-close:|
| EllipseAndFourCircles  | Four circles enclosed by an ellipse |:material-check: | :material-check:4 | :material-close: | :material-check: | :material-close: | :material-close: | :material-close: | :material-close:| :material-close:|
| EastCoastUS  | A region of the US East Coast |:material-check: |  :material-close: | :material-check: | :material-close: | :material-check:(From file) | :material-check: | :material-close: | :material-close:| :material-close:|
| GingerbreadMan  | Geometry with lots of holes |:material-check: | :material-check:6 | :material-close: | :material-check: | :material-check: | :material-close: | :material-close: | :material-close:| :material-close:|
| Half Circle  | A half circle  |:material-check: | :material-close: | :material-close: | :material-check: | :material-close: | :material-check: | :material-close: | :material-close:| :material-close:
| Indian Ocean  | Complex domain with islands and inlets  |:material-check: | :material-check:3 | :material-close: | :material-close: | :material-check: | :material-close: | :material-close: | :material-close:| :material-close:
| KT3Element  | Three element Karman-Treffitz airfoil  |:material-close: | :material-check:3 | :material-close: | :material-close: | :material-check: | :material-close: | :material-close: | :material-close:| :material-close:
| Lake Superior  | Complex domain with islands  |:material-check: | :material-check:2 | :material-close: | :material-close: | :material-check: | :material-close: | :material-close: | :material-close:| :material-close:
| NACA0012  | Standard Airfoil geometry  |:material-close: | :material-check: | :material-close: | :material-check: | :material-close: | :material-close: | :material-close: | :material-check:| :material-close:
| Pill  | Oblong domain with interior circles  |:material-check: | :material-check:3 | :material-check: | :material-close: | :material-close: | :material-close: |:material-close: | :material-close:| :material-close: 
| SplineGeometry  | Free form domain defined as a spline  |:material-check: | :material-close: | :material-close: | :material-close: | :material-check: | :material-close: |:material-close: | :material-close:| :material-close: 
| Square  | Generation of a Cartesian mesh with no model  |:material-close: | :material-close: | :material-close: | :material-close: | :material-close: | :material-close: | :material-close: | :material-close:| :material-close: 

There are also eight three dimensional examples that illustrate the different extrusion algorithms in the Examples/3D directory

| Name | Description | Algorithm| Topography |
|------|-------------|----------|------------|
| Box  | Extrusion of a square into a box. *No Model* | SIMPLE_EXTRUSION | :material-close: |
| BoxRotated  | Rotation of a square about the x-axis. *No Model* | SIMPLE_ROTATION | :material-close: |
| CavityRampExtruded  | Extrusion of the 2D CavityRamp| SIMPLE_EXTRUSION | :material-close: |
| HalfCircleExtruded  | Extrusion of the 2D HalfCircle| SIMPLE_EXTRUSION | :material-close: |
| HalfCircleRotated  | Rotation of the 2D half circle about the x-axis| SIMPLE_ROTATION | :material-close: |
| Pond  | Extrusion of a circle| SIMPLE_EXTRUSION | :material-check: |
| ScaledCylinder  | Sweeping of a circle with scale factor applied| SWEEP\_ALONG_CURVE | :material-close: |
| Snake  | Sweeping of a circle along multiple axes| SWEEP\_ALONG_CURVE | :material-close: |
