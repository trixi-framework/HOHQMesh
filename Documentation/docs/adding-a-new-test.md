When a new feature is added to HOHQMesh it is best practice to add a (small) corresponding test case
control file that exercises these new routines. Adding a new test is straightforward with the following
procedure:

1. Create a new control file, e.g., `NewTestMesh.control` and save it in the folder `/Benchmarks/ControlFiles`. This
   is the version of the control file that HOHQMesh will execute for testing.
2. In this new control file add the keyword "test file name" with an appropriate file path and name corresponding to the test
   to the RUN_PARAMETERS block of the control file. For example, in `NewTestMesh.control`
   ```
   test file name = /Benchmarks/BenchmarkData/NewTestMesh.txt
   ```
   will indicate that the benchmark information should be written to the file `NewTestMesh.txt`.
   The file path `/Benchmarks/BenchmarkData` is the expected path for the automated tests.
3. Generate the benchmark information by adding the `-generateTest` flag, i.e.
   ```
   ./HOHQMesh -generateTest -f Benchmarks/ControlFiles/NewTestMesh.control
   ```
   This will write the test benchmark file to the location requested in the previous step.
4. Open the file `/Benchmarks/BenchmarkFiles.txt` and add the new control file path to the list of test cases, e.g.,
   ```
   Benchmarks/ControlFiles/NewTestMesh.control
   ```

To execute the tests locally type
```
./HOHQMesh -test -path <pathToBenchmarks>
```
where `<pathToBenchmarks>` is the path to the HOHQMesh directory. If you are inside the HOHQMesh directory, you can also omit the `-path` option, as it defaults to `.`.

## Updating the benchmarks

Changes to the meshing algorithm components (smoothing, sizing, boundary handling, topology cleanup, etc.) can change the meshes so that the existing benchmark data no longer matches.
After building HOHQMesh run all the tests from the home directory with
```
./HOHQMesh -test
```
Inspect any failing tests and the corresponding benchmark quantities that are altered.
In particular, one should be vigilant to examine these changes and determine if they
are reasonable and expected due to the updated meshing algorithms.

The benchmarks can be regenerated with the script `Utilities/updatebenchmarks`.
Run it from the HOHQMesh directory after a build, either for all the test cases listed in
`/Benchmarks/BenchmarkFiles.txt`
```
./Utilities/updatebenchmarks
```
or only for a selected control file, e.g.,
```
./Utilities/updatebenchmarks Benchmarks/ControlFiles/NewTestMesh.control
```
For each control file the script runs HOHQMesh with the `-generateTest` flag, which rewrites the
benchmark information in `/Benchmarks/BenchmarkData`.
It then copies the plot and stats files written to `/Benchmarks/PlotFiles/Tests` and `/Benchmarks/StatsFiles/Tests` into the reference folders
`/Benchmarks/PlotFiles/Benchmarks` and `/Benchmarks/StatsFiles/Benchmarks`, and finally lists the
changed files.
Check these changes, e.g., with `git diff --stat Benchmarks`, before committing them.
