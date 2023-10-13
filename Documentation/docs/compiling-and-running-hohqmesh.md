# Compiling and Running HOHQMesh
## Quick Introduction <a name="Compiling"></a>
HOHQMesh is currently being distributed on gitHub. In that repository is a makefile, plus source, documentation (which contains this document) and a directory of examples.

HOHQMesh is written in fortran 2018. It is known to compile and run with `gfortran` on Mac/Linux/Windows and with `ifort` on Mac and Linux. At the time of this writing, HOHQMesh compiles, but does not run correctly, with the nFortran compiler from nvidia. Let us know if there are issues with other compilers, or more recent versions of those.

The mesher has one dependency, FTObjectLibrary, which supplies the container classes and exception classes. It is also available on gitHub under a project of that name.

The makefile is for gmake. Use it to compile the mesher.

To build, edit the `Makefile` file as indicated in the header and move to
ones favorite directory. Type
```
make
```
That will build HOHQMesh.

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

To mesh a control file, type

```bash
./HOHQMesh -f <pathToControlFile>
```

For example, to mesh the GingerbreadMan model in the Examples directory, type
```bash
./HOHQMesh -f Examples/2D/GingerbreadMan/GingerbreadMan.control
```


The mesh and plot files will be created relative to the directory of the executable. For the moment, until things get really robust, diagnostic information can be printed as the program executes.

Three more compiler flags are also defined:

* **-version**		Gives the version number of the code
* **-help**			Does nothing at the moment. Sorry. RTM.
* **-verbose**		Determines whether progress messages are printed or not.

Use these as usual, e.g.
```bash
./HOHQMesh -verbose -f Examples/2D/GingerbreadMan/GingerbreadMan.control
```