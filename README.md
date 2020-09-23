# HOHQMesh

## About

## Installation

### CMake
`HOHQMesh` depends on the [FTObjectLibrary](https://bitbucket.org/DavidKoprivaFSU/ftobjectlibrary).

To install the FTObjectLibrary with CMake,
```
git clone git@bitbucket.org:DavidKoprivaFSU/ftobjectlibrary.git
cd ftobjectlibrary
git checkout feature/cmake_build
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/hohqmesh ../
make
sudo make install
```
This installs `/opt/hohqmesh/lib/libftobject.a` and the compiled `.mod` files under `/opt/hohqmesh/include`  

To build the HOHQMesh application
```
mkdir build && cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/hohqmesh -DFTOBJ_PREFIX=/opt/hohqmesh ../
make
sudo make install
```
This installs `/opt/hohqmesh/bin/HOHQMesh`, `/opt/hohqmesh/lib/libhohqmesh.a`, and the compiled `.mod` files under `/opt/hohqmesh/include`. 


