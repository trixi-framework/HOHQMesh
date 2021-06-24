# HOHQMesh

[![Build Status](https://github.com/trixi-framework/HOHQMesh/workflows/CI/badge.svg)](https://github.com/trixi-framework/HOHQMesh/actions?query=workflow%3ACI)
[![Coveralls](https://coveralls.io/repos/github/trixi-framework/HOHQMesh/badge.svg?branch=main)](https://coveralls.io/github/trixi-framework/HOHQMesh?branch=main)
[![Codecov](https://codecov.io/gh/trixi-framework/HOHQMesh/branch/main/graph/badge.svg)](https://codecov.io/gh/trixi-framework/HOHQMesh)
[![License: MIT](https://img.shields.io/badge/License-MIT-success.svg)](https://opensource.org/licenses/MIT)

<p align="center">
  <img width="400px" src="https://user-images.githubusercontent.com/3637659/121870408-50418800-cd03-11eb-9187-dcafdf73bab2.png" />
</p>

## Getting started



### Install with Spack
You can install HOHQMesh using the [Spack package manager](https://spack.io). 
To install the HOHQMesh with Spack,
```
git clone https://github.com/spack/spack.git ~/spack
source ~/spack/share/spack/setup-env.sh
spack install hohqmesh@main
```
This will install HOHQMesh and all of it's dependencies (including FTObjectLibrary) from source code.
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
The compiler choice can be overriden by passing `FC=<pathToCompiler>` to
`make`.
You can further pass the `-jN` option to `make` (with `N` being a non-negative
integer), which will use `N` parallel processes.

For example, to build HOHQMesh specifically with the Fortran compiler
`gfortran-10` and with 4 parallel processes, execute
```bash
make -j 4 FC=gfortran-10
```

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


### Updating Spack Packages
When new releases of HOHQMesh are made, you can contribute the new release to the spack packages.
Before making an update to the Spack package, you should familiarize yourself with [the Spack packaging guide](https://spack.readthedocs.io/en/latest/packaging_guide.html)


To add a new HOHQMesh release to Spack,
1. Fork the Spack repository on Github to your personal account or the trixi-framework organization account.

2. Install and initialize spack on your local system from your fork
```
git clone https://github.com/YOUR GITHUB ACCOUNT/spack.git ~/spack
source ~/spack/share/spack/setup-env.sh
```

3. Open the HOHQMesh package for editing
```
spack edit hohqmesh
```

4. Add a new `version` metadata item to the hohqmesh package. The first argument is the version name as it will appear in the spack package manager. Use the `tag` argument to specify the name of the tag as it appears in the HOHQMesh repository. As an example, a new version line for `v1.0.1` is shown below.
```
    version('v1.0.1', tag='v1.0.1')
```

5. (Optional) If you would like to be noted as a maintainer, add your github handle to the maintainers list. Maintainers will be notified if Spack users experience issues installing HOHQMesh and when modifications are being made to the HOHQMesh package in Spack.
```
    maintainers = ['schoonovernumerics','your-github-handle']
```

6. When you are finished editing, save the package file. You can verify the new version is registered in your local repository by obtaining a `spec` for HOHQMesh at the new version you've added.
```
spack spec hohqmesh@v1.0.1
```

7. Test to make sure the installation works
```
spack install hohqmesh@v1.0.1
```

8. Run the `spack style` command to ensure that you are meeting Spack's style requirements.

9. When ready, commit your changes and push them to your Github repository.

10. Open a Pull Request with `github.com/spack/spack` to merge your changes with the `spack/spack/develop` branch.


## Authors
HOHQMesh was initiated by
[David A. Kopriva](https://www.math.fsu.edu/~kopriva/), who is also the principal developer.
The full list of contributors can be found in [AUTHORS.md](AUTHORS.md).


## License and contributing
HOHQMesh is licensed under the MIT license (see [LICENSE.md](LICENSE.md)).
