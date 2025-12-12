\name{mem_parallel_info}
\alias{mem_parallel_info}
\title{Get information about current parallel setup}
\usage{
mem_parallel_info()
}
\value{
Invisibly returns a list with parallel backend information including:
\describe{
  \item{future_available}{Whether the future package is installed}
  \item{parallelly_available}{Whether the parallelly package is installed}
  \item{plan}{Current future plan (e.g., "multisession", "sequential")}
  \item{n_workers}{Number of configured workers}
  \item{worker_pids}{Integer vector of detected worker PIDs}
  \item{child_pids}{Integer vector of child process PIDs}
  \item{main_pid}{PID of the main R process}
}
}
\description{
Returns diagnostic information about the detected parallel backend
and any workers that can be monitored. Useful for verifying that
memtoc can detect your parallel workers before running a job.
}
\examples{
mem_parallel_info()

# With future workers active:
# library(future)
# plan(multisession, workers = 2)
# mem_parallel_info()
}
\seealso{
\code{\link{tic_mem}} for the \code{workers} parameter,
\code{\link{mem_capabilities}} for general capability check
}
