# memtoc

<!-- badges: start -->
[![R-CMD-check](https://github.com/jcoa05/memtoc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jcoa05/memtoc/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**Tictoc-style memory tracking for R.** Simple start/stop syntax for monitoring RAM usage during code execution, with continuous background polling to capture true peak memory.

Inspired by the [tictoc](https://github.com/jabiru/tictoc) package for timing.

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("jcoa05/memtoc")

# Or using devtools
devtools::install_github("jcoa05/memtoc")
```

## Quick Start

```r
library(memtoc)

# Track memory for any operation
tic_mem("data processing")
data <- read.csv("large_file.csv")
processed <- transform(data)
toc_mem()
#> ✔ data processing: 142.3 MB peak | 89.1 MB current | 2.34 sec | 3 samples
```

## Why memtoc?

R's built-in memory tools (`gc()`, `object.size()`) only show point-in-time snapshots. **memtoc captures the true peak memory** even for short-lived allocations by continuously sampling in the background.

```r
tic_mem("matrix operation")
x <- matrix(rnorm(1e8), ncol = 1000)  # ~800 MB temporary allocation
y <- colMeans(x)                        
rm(x)  # x is gone, but memtoc caught the peak!
toc_mem()
#> ✔ matrix operation: 812.4 MB peak | 45.2 MB current | 3.21 sec | 7 samples
```

Without background polling, you'd only see the final 45 MB.

## Features

| Feature | Description |
|---------|-------------|
| 🎯 **Background Polling** | Continuous sampling catches transient allocations |
| 📊 **Nested Tracking** | Track pipelines and individual steps simultaneously |
| ⚡ **Parallel Monitoring** | Auto-detect and monitor `future` workers |
| 💾 **Crash Recovery** | Recover data if R crashes mid-computation |
| ⚠️ **System Warnings** | Alerts when system RAM is running low |
| 📝 **Logging** | Collect results for later analysis |

### Background Polling

```r
tic_mem("job", interval = 0.5)  # Sample every 0.5 seconds
# ... your code ...
result <- toc_mem()
result$trajectory  # Full memory timeline
```

### Nested Tracking

```r
tic_mem("full pipeline")
  tic_mem("step 1"); do_step1(); toc_mem()
  tic_mem("step 2"); do_step2(); toc_mem()
  tic_mem("step 3"); do_step3(); toc_mem()
toc_mem()
```

### Parallel Worker Monitoring

```r
library(future)
plan(multisession, workers = 4)

tic_mem("parallel job", workers = "auto")
result <- future_lapply(1:100, heavy_function)
toc_mem()
#> ✔ parallel job: 1.2 GB peak | 245 MB current | 5.4 sec | 4 workers
```

### Crash Recovery

```r
# After R restart
mem_recover()
#> ℹ Found 1 recovery file: PID 12345 (152 samples)
data <- mem_recover(pid = 12345)
```

## API Reference

| Function | Description |
|----------|-------------|
| `tic_mem()` | Start tracking |
| `toc_mem()` | Stop tracking and report results |
| `mem_log()` | Get logged results as data frame |
| `mem_clearlog()` | Clear the log |
| `mem_clear()` | Clear orphaned tracking entries |
| `mem_recover()` | Recover data from crashed sessions |
| `mem_capabilities()` | Check available features |
| `mem_diagnose()` | Detailed troubleshooting |
| `mem_parallel_info()` | Check parallel backend status |

## Documentation

See `vignette("memtoc")` for a detailed tutorial, or `?tic_mem` for function help.

## Requirements

- R ≥ 4.1.0
- Dependencies: `ps`, `cli`, `callr` (installed automatically)
- Optional: `future`, `parallelly` for parallel worker monitoring

## Related Packages

- [tictoc](https://github.com/jabiru/tictoc): Timing (memtoc is for memory)
- [bench](https://bench.r-lib.org/): Benchmarking with memory tracking
- [profmem](https://github.com/HenrikBengtsson/profmem): Memory profiling

## Contributing

Issues and pull requests welcome at [GitHub](https://github.com/jcoa05/memtoc/issues).

## License

MIT
