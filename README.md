# HOHQMesh

[![Build Status](https://github.com/trixi-framework/HOHQMesh/workflows/CI/badge.svg)](https://github.com/trixi-framework/HOHQMesh/actions?query=workflow%3ACI)
[![Coveralls](https://coveralls.io/repos/github/trixi-framework/HOHQMesh/badge.svg?branch=main)](https://coveralls.io/github/trixi-framework/HOHQMesh?branch=main)
[![Codecov](https://codecov.io/gh/trixi-framework/HOHQMesh/branch/main/graph/badge.svg)](https://codecov.io/gh/trixi-framework/HOHQMesh)
[![License: MIT](https://img.shields.io/badge/License-MIT-success.svg)](https://opensource.org/licenses/MIT)

<p align="center">
  <img width="400px" src="https://user-images.githubusercontent.com/3637659/121870408-50418800-cd03-11eb-9187-dcafdf73bab2.png" />
</p>

## Installation
To build, edit the `Makefile` file as indicated in the header and move to
ones favorite directory. Type
```shell
make
```
That will build HOHQMesh.

To run the tests, type
```bash
./HOHQMesh -test -path <pathToBenchmarks>
```
where `<pathToBenchmarks>` is the path to the HOHQMesh directory.

To mesh a control file, type
```bash
./HOHQMesh -f <pathToControlFile>
```

## Authors
HOHQMesh was initiated by
[David A. Kopriva](https://www.math.fsu.edu/~kopriva/), who is also the principal developer.
The full list of contributors can be found in [AUTHORS.md](AUTHORS.md).


## License and contributing
HOHQMesh is licensed under the MIT license (see [LICENSE.md](LICENSE.md)).
