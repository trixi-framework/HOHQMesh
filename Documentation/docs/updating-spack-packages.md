# Updating Spack Packages
When new releases of HOHQMesh are made, you can contribute the new release to the spack packages.
Before making an update to the Spack package, you should familiarize yourself with [the Spack packaging guide](https://spack.readthedocs.io/en/latest/packaging_guide.html).


To add a new HOHQMesh release to Spack, perform the following steps:

1. Fork the Spack repository on GitHub.

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

5. (Optional) If you would like to be noted as a maintainer, add your GitHub handle to the maintainers list. Maintainers will be notified if Spack users experience issues installing HOHQMesh and when modifications are being made to the HOHQMesh package in Spack.
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

9. When ready, commit your changes and push them to your GitHub repository.

10. Open a Pull Request against `github.com/spack/spack` to merge your changes with the `spack/spack/develop` branch.

