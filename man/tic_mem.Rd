\name{tic_mem}
\alias{tic_mem}
\title{Start memory tracking}
\usage{
tic_mem(msg = NULL, quiet = TRUE, interval = 1, persist = TRUE, workers = "auto")
}
\arguments{
\item{msg}{Optional character string label for this tracking block.
This label is displayed in the output from \code{toc_mem()} and stored
in the log if \code{log = TRUE} is passed to \code{toc_mem()}.}

\item{quiet}{Logical. If \code{TRUE} (default), no message is printed when
tracking starts. Set to \code{FALSE} to see a startup message.}

\item{interval}{Numeric. Polling interval in seconds for background
monitoring. Set to \code{NULL} or \code{0} to disable background polling and
use snapshot-only mode (faster but only captures start/end memory).
Default is \code{1} second.}

\item{persist}{Logical. If \code{TRUE} (default), periodically saves samples
to disk for crash recovery. Set to \code{FALSE} to disable persistence.}

\item{workers}{Worker specification for parallel monitoring:
\itemize{
  \item \code{"auto"} (default): Automatically detect future workers and child processes
  \item \code{"none"}: Only monitor the main R process
  \item \code{"children"}: Monitor main process and all child processes
  \item Integer vector: Explicit list of PIDs to monitor
}}
}
\value{
Invisibly returns the timestamp when tracking started.
}
\description{
Begins a memory tracking block. Call \code{\link{toc_mem}} to end the block and
see the results. Multiple calls to \code{tic_mem()} can be nested, and each
\code{toc_mem()} will match with the most recent unmatched \code{tic_mem()}.
}
\details{
When \code{interval} is set (default), a background R process is spawned to
continuously sample memory usage. This captures the true peak memory
even for short-lived allocations. The background process adds minimal
overhead (typically less than 1\% CPU).

For very short operations (under 1 second), consider using \code{interval = NULL}
to avoid the background process startup overhead (~200ms).

\subsection{Parallel Worker Monitoring}{
When using \code{future} for parallel processing, memtoc can automatically
detect and monitor worker processes. Set \code{workers = "auto"} to enable
this feature. The trajectory will include memory samples from all
workers, and the result will show aggregate statistics.
}
}
\examples{
# Basic usage with background polling (default)
tic_mem("my operation")
Sys.sleep(0.5)
x <- numeric(1e6)
toc_mem()

# Snapshot-only mode (no background process)
tic_mem("quick op", interval = NULL)
y <- 1:100
toc_mem()

# Custom polling interval
tic_mem("long operation", interval = 0.5)
Sys.sleep(2)
toc_mem()

# Monitor with parallel workers (if future is set up)
# future::plan(future::multisession, workers = 2)
# tic_mem("parallel job", workers = "auto")
# result <- future.apply::future_lapply(1:10, function(x) rnorm(1e6))
# toc_mem()

# Nested tracking
tic_mem("outer")
  tic_mem("inner", interval = NULL)
  Sys.sleep(0.1)
  toc_mem()
toc_mem()
}
\seealso{
\code{\link{toc_mem}} to stop tracking, \code{\link{mem_log}} to retrieve logged results,
\code{\link{mem_recover}} to recover data after a crash,
\code{\link{mem_parallel_info}} to check parallel backend status
}
