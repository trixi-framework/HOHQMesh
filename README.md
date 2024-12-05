# HOHQMesh

[![Docs-stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://trixi-framework.github.io/HOHQMesh)
[![Build Status](https://github.com/trixi-framework/HOHQMesh/workflows/CI/badge.svg)](https://github.com/trixi-framework/HOHQMesh/actions?query=workflow%3ACI)
[![Coveralls](https://coveralls.io/repos/github/trixi-framework/HOHQMesh/badge.svg?branch=main)](https://coveralls.io/github/trixi-framework/HOHQMesh?branch=main)
[![Codecov](https://codecov.io/gh/trixi-framework/HOHQMesh/branch/main/graph/badge.svg)](https://codecov.io/gh/trixi-framework/HOHQMesh)
[![License: MIT](https://img.shields.io/badge/License-MIT-success.svg)](https://opensource.org/licenses/MIT)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13959058.svg)](https://doi.org/10.5281/zenodo.13959058)

<p align="center">
  <img width="400px" src="https://user-images.githubusercontent.com/3637659/121870408-50418800-cd03-11eb-9187-dcafdf73bab2.png" />
</p>

**HOHQMesh**, the *High Order Hex-Quad Mesher*, is an open-source mesh generator
that automatically creates quadrilateral/hexahedral meshes with high-order boundary
information. To get an impression of what kind of meshes HOHQMesh can generate,
please see the [gallery](https://trixi-framework.github.io/HOHQMesh/Gallery/).

## Getting started

HOHQMesh can be used via
[HOHQMesh.jl](https://github.com/trixi-framework/HOHQMesh.jl), a
[Julia](https://julialang.org) package that provides an interface to HOHQMesh and that
supplies precompiled executables for Linux, macOS, Windows, and
FreeBSD. If you would like to use HOHQMesh directly from the command line,
please continue reading the next sections for instructions on how to obtain
the sources and compile HOHQMesh yourself.


### Install with Spack
You can install HOHQMesh using the [Spack package manager](https://spack.io).
To install the HOHQMesh with Spack,
```
git clone https://github.com/spack/spack.git ~/spack
source ~/spack/share/spack/setup-env.sh
spack install hohqmesh@main
```
This will install HOHQMesh and all of its dependencies (including FTObjectLibrary) from source code.
Once installed, HOHQMesh can be added to your environment using
```
spack load hohqmesh
```


### Obtaining the sources
You can download the
[latest HOHQMesh release](https://github.com/trixi-framework/HOHQMesh/releases/latest)
from GitHub. Make sure to get the tarball named `HOHQMesh-vVERSION.tar.gz`, as
it already contains the required sources for the
[FTObjectLibrary](https://github.com/trixi-framework/FTObjectLibrary)
dependency, and unpack it with `tar xf HOHQMesh-vVERSION.tar.gz`.
Alternatively, you can build HOHQMesh directly from the latest sources in the
`main` branch. In this case, you need enter the clone directory and execute
```bash
./Utilities/bootstrap
```
before proceeding, which will download the `FTObjectLibrary` sources for you.
This step is required only once.

### Building
Enter the HOHQMesh directory and execute
```shell
make
```
This will build HOHQMesh using the `gfortran` compiler by default.
The compiler choice can be overridden by passing `FC=<pathToCompiler>` to
`make`.
You can further pass the `-jN` option to `make` (with `N` being a non-negative
integer), which will use `N` parallel processes.

For example, to build HOHQMesh specifically with the Fortran compiler
`gfortran-10` and with 4 parallel processes, execute
```bash
make -j 4 FC=gfortran-10
```

HOHQMesh is tested to run with the `gfortran` and `ifort` compilers. We recommend the `gfortran` compiler. Our experience on the test suite is that it runs about 50% slower with the `ifort` compiler.

### Testing
After building HOHQMesh, you can verify that everything works as expected by
running the internal test suite. To execute the tests, type
```bash
./HOHQMesh -test -path <pathToBenchmarks>
```
where `<pathToBenchmarks>` is the path to the HOHQMesh directory. If you are
inside the HOHQMesh directory, you can also omit the `-path` option, as it
defaults to `.`.

### Generating a mesh
To mesh a control file, type
```bash
./HOHQMesh -f <pathToControlFile>
```
where `-f` allows you to provide the path to the control file for which you want
to create your mesh.


## Documentation
The full documentation on how to use HOHQMesh, including the preparation of input files and
the visualization of the resulting mesh files, can be found
[here](https://trixi-framework.github.io/HOHQMesh).


## Referencing
If you use HOHQMesh in your own research, please cite this repository as follows:
```bibtex
@misc{kopriva2024hohqmesh,
  title={{HOHQM}esh: An All Quadrilateral/Hexahedral Unstructured Mesh Generator for High Order Elements},
  author={Kopriva, David A and Winters, Andrew R and Schlottke-Lakemper, Michael
          and Schoonover, Joseph A and Ranocha, Hendrik},
  year={2024},
  howpublished={\url{https://github.com/trixi-framework/HOHQMesh}},
  doi={10.5281/zenodo.13959058}
}
```


## Authors
HOHQMesh was initiated by
[David A. Kopriva](https://www.math.fsu.edu/~kopriva/), who is also the principal developer.
The full list of contributors can be found in [AUTHORS.md](AUTHORS.md).


## License and contributing
HOHQMesh is licensed under the MIT license (see [LICENSE.md](LICENSE.md)).
