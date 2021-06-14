# Compiling and Running HOHQMesh
## Compiling the Mesher<a name="Compiling"></a>
HOHQMesh is currently being distributed on gitHub. In that repository is a makefile, plus source, documentation (which contains this document) and a directory of examples.

HOHQMesh is written in fortran90. It is known to compile and run with gfortran on Mac/Linux/Windows. Let us know if there are issues with other compilers. Or more recent versions of those.

The mesher has one dependency, FTObjectLibrary, which supplies the container classes and exception classes. It is also available on gitHub under a project of that name. 

The makefile is for gmake. Use it to compile the mesher.

To build, edit the `Makefile` file as indicated in the header and move to
ones favorite directory. Type
```
make
```
That will build HOHQMesh.


## Running the Mesher<a name="RunningIt"></a>


To run the tests, type

```bash
./HOHQMesh -test -path <pathToBenchmarks>
```
where `<pathToBenchmarks>` is the path to the HOHQMesh directory.

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
