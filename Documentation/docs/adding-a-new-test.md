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