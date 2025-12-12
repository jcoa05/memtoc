\name{memtoc-package}
\alias{memtoc-package}
\alias{memtoc}
\docType{package}
\title{Tictoc-Style Memory Usage Tracking}
\description{
The memtoc package provides simple start/stop memory tracking functions
that can be nested, inspired by the tictoc package for timing. Track RAM
usage during code execution with support for logging, custom messages,
background polling for true peak detection, and parallel worker monitoring.
}
\section{Main Functions}{
\itemize{
  \item \code{\link{tic_mem}}: Start memory tracking
  \item \code{\link{toc_mem}}: Stop memory tracking and report
  \item \code{\link{mem_log}}: Retrieve logged memory measurements
  \item \code{\link{mem_clearlog}}: Clear the memory log
  \item \code{\link{mem_clear}}: Clear the tracking stack
  \item \code{\link{mem_recover}}: Recover data from crashed sessions
  \item \code{\link{mem_capabilities}}: Check available features
  \item \code{\link{mem_diagnose}}: Detailed diagnostics
  \item \code{\link{mem_parallel_info}}: Check parallel backend status
}
}
\section{Basic Usage}{
\preformatted{
tic_mem("data loading")
data <- read.csv("large_file.csv")
toc_mem()
#> data loading: 142.3 MB peak | 89.1 MB current | 2.34 sec | 3 samples
}
}
\section{Background Polling}{
By default, memtoc spawns a background process to continuously sample
memory, capturing the true peak even for short-lived allocations:
\preformatted{
tic_mem("computation", interval = 0.5)  # Sample every 0.5 seconds
x <- matrix(rnorm(1e8), ncol = 1000)    # Large temporary allocation
result <- toc_mem()
result$trajectory  # Full memory trajectory
}
}
\section{Parallel Worker Monitoring}{
When using the future package, memtoc can monitor memory across workers:
\preformatted{
library(future)
plan(multisession, workers = 4)
tic_mem("parallel job", workers = "auto")
# ... parallel computation ...
result <- toc_mem()
result$worker_stats  # Per-worker memory usage
}
}
\section{Nested Tracking}{
\preformatted{
tic_mem("full pipeline")
  tic_mem("preprocessing")
  # ... code ...
  toc_mem()

  tic_mem("modeling")
  # ... code ...
  toc_mem()
toc_mem()
}
}
\author{
Maintainer: Your Name <your.email@example.com>
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/yourusername/memtoc}
  \item Report bugs at \url{https://github.com/yourusername/memtoc/issues}
}
}
