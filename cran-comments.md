## R CMD check results

0 errors | 0 warnings | 0 note

* This is a new submission.

## Test environments

* Local Windows 11, R 4.5.1
* GitHub Actions (windows-latest)

## Package Description

memtoc provides tictoc-style memory tracking for R. It allows users to wrap
code blocks with `tic_mem()` / `toc_mem()` to measure RAM usage, similar to
how the tictoc package measures execution time.

Key features:
- Simple start/stop API inspired by tictoc
- Background polling to capture true peak memory
- Support for nested tracking blocks
- Parallel worker monitoring (future package integration
- Crash recovery via persistent checkpoints

## Dependencies

The package has minimal dependencies:
- ps: For cross-platform memory queries
- cli: For formatted console output
- callr: For background process management

Optional suggests (future, parallelly) are only used for parallel worker
detection and are not required for core functionality.

## Downstream dependencies

This is a new package with no reverse dependencies.
