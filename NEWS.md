# memtoc 0.4.0

## Additions

* Added vignette.
* Added NEWS.md changelog.
* Package passes R CMD check with no errors or warnings.

# memtoc 0.3.1

## Bug Fixes

* Fixed background monitor sample retrieval on Windows. The monitor now saves
  checkpoints on every sample and reads from the persistence file when the
  background process is terminated.
* Fixed test script using unexported `format_bytes()` function.

# memtoc 0.3.0

## New Features
 
* **Parallel worker monitoring**: New `workers` parameter in `tic_mem()` allows
 monitoring memory across parallel workers from the `future` package ecosystem.
  - `workers = "auto"`: Automatically detect future workers and child processes
  - `workers = "none"`: Only monitor the main R process
  - `workers = "children"`: Monitor main process and all child processes
  - Integer vector: Explicit list of PIDs to monitor

* **Per-worker statistics**: Results now include `worker_stats` data frame with
  peak memory usage for each monitored process.

* **New function `mem_parallel_info()`**: Diagnostic function to check parallel
  backend status and detected workers before running a job.

* Results now include `n_workers` field showing number of worker processes
  monitored.

# memtoc 0.2.0

## New Features

* **Background polling**: New `interval` parameter in `tic_mem()` enables
  continuous memory sampling via a background R process. This captures true
  peak memory even for short-lived allocations.

* **Memory trajectory**: Results now include a `trajectory` data frame with
 timestamped memory samples when background polling is enabled.

* **System memory context**: Trajectory includes system-wide RAM usage
  (`sys_total`, `sys_avail`, `sys_percent`). Warnings are displayed when
  system RAM exceeds 80% or 95%.

* **CPU time tracking**: Results include `cpu_user` and `cpu_system` fields
  showing CPU time consumed during the tracked interval.

* **Crash recovery**: New `mem_recover()` function retrieves memory samples
  from a crashed R session using persisted checkpoint files.

* **Capability detection**: New `mem_capabilities()` function checks which
  features are available on the current system.

* **Diagnostics**: New `mem_diagnose()` function provides detailed
  troubleshooting when background polling is unavailable.

# memtoc 0.1.0

## Initial Release

* Core `tic_mem()` / `toc_mem()` functions for start/stop memory tracking.
* Nested tracking support with proper stack management.
* Logging with `mem_log()`, `mem_clearlog()`, and `mem_print_log()`.
* Human-readable output with peak memory, current memory, and elapsed time.
* Print method for result objects showing detailed breakdown.
