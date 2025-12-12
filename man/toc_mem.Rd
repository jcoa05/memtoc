\name{toc_mem}
\alias{toc_mem}
\title{Stop memory tracking and report results}
\usage{
toc_mem(log = FALSE, quiet = FALSE)
}
\arguments{
\item{log}{Logical. If \code{TRUE}, stores the result in an internal log that
can be retrieved later with \code{\link{mem_log}}. Default is \code{FALSE}.}

\item{quiet}{Logical. If \code{TRUE}, suppresses the output message.
Default is \code{FALSE} (message is shown).}
}
\value{
Invisibly returns a \code{memtoc_result} object (a list) containing:
\describe{
  \item{msg}{The label passed to \code{tic_mem()}, or NULL}
  \item{mem_start}{Memory (RSS) in bytes at start (main process)}
  \item{mem_end}{Memory (RSS) in bytes at end (main process)}
  \item{mem_peak}{Peak memory during the interval (total across all processes)}
  \item{mem_change}{Change in memory (end - start) in bytes}
  \item{elapsed}{Elapsed time in seconds}
  \item{tic_timestamp}{POSIXct timestamp when tic_mem() was called}
  \item{toc_timestamp}{POSIXct timestamp when toc_mem() was called}
  \item{trajectory}{Data frame of memory samples (if background polling was used)}
  \item{n_samples}{Number of samples collected}
  \item{n_workers}{Number of worker processes monitored (0 if main process only)}
  \item{worker_stats}{Data frame with per-worker peak memory (if workers monitored)}
  \item{sys_peak_percent}{Peak system RAM usage percentage}
  \item{sys_total}{Total system RAM in bytes}
  \item{cpu_user}{User CPU time consumed (seconds)}
  \item{cpu_system}{System CPU time consumed (seconds)}
}
}
\description{
Ends a memory tracking block started by \code{\link{tic_mem}} and reports the results.
By default, prints a summary message showing peak memory, current memory,
and elapsed time.
}
\examples{
tic_mem("example")
x <- rnorm(1e6)
Sys.sleep(1)
result <- toc_mem()

# Access result components
result$elapsed
result$mem_peak
result$n_samples

# View the full trajectory (if available)
if (!is.null(result$trajectory)) {
  head(result$trajectory)
}

# View per-worker stats (if workers were monitored)
if (!is.null(result$worker_stats)) {
  result$worker_stats
}
}
\seealso{
\code{\link{tic_mem}} to start tracking, \code{\link{mem_log}} to retrieve logged results
}
