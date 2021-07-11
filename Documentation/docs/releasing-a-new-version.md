# Releasing a New Version

This document describes the necessary steps for creating a new HOHQMesh release.
When following these guidelines, don't forget to replace `1.2.3` with the
current release version of HOHQMesh, and `1.3.0` with the new release you are
about to create.

*Note: All steps as described here are assumed to be performed in a
clean clone of the `main` branch. Otherwise the creation of the release
tarball will fail!*

## Pre-release preparations
1. Ensure that the current HOHQMesh commit on `main` has passed all tests on
   GitHub by verifying that there is a green checkmark behind the latest commit
   on https://github.com/trixi-framework/HOHQMesh/commits/main.
2. Enter the HOHQMesh clone directory, switch to `main`, and pull the latest version from GitHub:
   ```bash
   cd HOHQMesh
   git checkout main
   git pull
   ```
3. Ensure that the latest version of `FTObjectLibrary` is present in the `Contrib`
   directory by calling the `bootstrap` script:
   ```bash
   ./Utilities/bootstrap
   ```
4. Get the latest HOHQMesh version tag by executing
   ```bash
   git tag --list 'v*' --sort -version:refname | head -n1
   ```
   This will yield something like `v1.2.3`, where `1.2.3` is the version number
   and the `v` prefix is to indicate that this is really a version tag.
5. Compare this with the
   [latest release](https://github.com/trixi-framework/HOHQMesh/releases/latest)
   on GitHub. If the version numbers of the latest tag and the latest release
   differ, investigate (and possibly fix it) before proceeding.
6. Determine which kind of version increment is necessary based on the changes
   since the last release. We use [semantic versioning](https://semver.org),
   which [states](https://semver.org/spec/v2.0.0.html#summary)
   > Given a version number MAJOR.MINOR.PATCH, increment the:
   >
   > MAJOR version when you make incompatible API changes,  
   > MINOR version when you add functionality in a backwards compatible manner, and  
   > PATCH version when you make backwards compatible bug fixes.

   To see all the changes since the last release, you can use GitHub's `compare`
   functionality by navigating to the following address, adjusting the version
   tag appropriately:  
   https://github.com/trixi-framework/HOHQMesh/compare/v1.2.3...main
   If you prefer to work to work locally in a console, you can execute
   ```bash
   git diff v1.2.3
   ```
   Let's say that the changes require a *minor* version increment, thus the new
   release version following `1.2.3` would be `1.3.0`.

## Release creation
1. Increment the version number in `Source/HOHQMeshMain.f90`.
   The current version in the file should be something like `1.2.4-pre`,
   indicating that the `main` branch is in a pre-release state (as it should be
   during development). Change the pre-release version string to the new version
   number you determined above, e.g., to `1.3.0`:
   ```fortran
   CHARACTER(LEN=*), PARAMETER :: version           = "1.3.0"
   ```
2. Commit and push to `main` (make sure to use *your* version number!):
   ```bash
   git add Source/HOHQMeshMain.f90
   git commit -m 'Increment version to v1.3.0'
   git push
   ```
3. Ensure that you did not accidentally break anything and verify that there is
   a green checkmark behind the latest commit on
   https://github.com/trixi-framework/HOHQMesh/commits/main.
4. Create a new release tarball by executing
   ```bash
   ./Utilities/createrelease 1.3.0
   ```
   where `1.3.0` again refers to the new release version (*without* the `v`
   prefix!). This will result in a new file `HOHQMesh-v1.3.0.tar.gz` in the
   current directory.
5. Test the new release by executing
   ```bash
   FC=gfortran ./Utilities/testrelease HOHQMesh-v1.3.0.tar.gz
   ```
   Make sure you change the Fortran compiler executable to one suitable
   for your system by modifying the `FC` environment variable accordingly.
   If it fails, do *not* just change the files in your current directory!
   Instead, figure out why the tests fail, fix them, commit and push the changes
   and start over.
6. Create a new annotated Git tag and push it to GitHub (make sure to include
   the `v` prefix and to use *your* version number both for the tag and the
   message!):
   ```bash
   git tag -a v1.3.0 -m "HOHQMesh v1.3.0"
   git push --tags
   ```
7. Navigate to https://github.com/trixi-framework/HOHQMesh/releases/new to start
   creating a new release.
8. Enter `v1.3.0` as the `Tag version`, which will should find the Git tag you
   just created.
9. Use `HOHQMesh v1.3.0` as the `Release title`.
10. Feel free to add more information about this release in the description field
   (optional).
11. Attach the file you just created to the new release by dragging and
    dropping the tarball `HOHQMesh-v1.3.0.tar.gz` in the gray area below the
    release description.
12. Click `Publish release`.


## Post-release actions
1. Increment the version number in `Source/HOHQMeshMain.f90` to a pre-release
   version to indicate that the current content of the `main` branch does not
   necessarily reflect the files of an existing, tagged release. The pre-release
   version is generated by incrementing the current patch version and appending `-pre`.
   For example, if you just created the release `1.3.0`, the next pre-release
   version would be `1.3.1-pre`.
2. Commit and push to `main` (make sure to use *your* version number!):
   ```bash
   git add Source/HOHQMeshMain.f90
   git commit -m 'Set development version to v1.3.1-pre'
   git push
   ```
3. Consider updating the [Spack package specifications](updating-spack-packages.md).
