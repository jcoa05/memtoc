\name{mem_log}
\alias{mem_log}
\title{Retrieve the memory tracking log}
\usage{
mem_log(format = c("data.frame", "list"))
}
\arguments{
\item{format}{Character string specifying the output format.
\describe{
  \item{"data.frame"}{(Default) Returns a data frame with one row per logged result}
  \item{"list"}{Returns the raw list of memtoc_result objects}
}}
}
\value{
A data frame (default) or list containing logged results. The data
frame has columns:
\describe{
  \item{msg}{Label from tic_mem(), or NA if none provided}
  \item{mem_start}{Memory (RSS) in bytes at start}
  \item{mem_end}{Memory (RSS) in bytes at end}
  \item{mem_peak}{Peak memory in bytes}
  \item{mem_change}{Change in memory (bytes)}
  \item{elapsed}{Elapsed time in seconds}
  \item{tic_timestamp}{When tic_mem() was called}
  \item{toc_timestamp}{When toc_mem() was called}
}
}
\description{
Returns a data frame containing all logged memory tracking results.
Results are added to the log when \code{toc_mem(log = TRUE)} is called.
}
\examples{
mem_clearlog()

tic_mem("step 1")
Sys.sleep(0.1)
toc_mem(log = TRUE, quiet = TRUE)

tic_mem("step 2")
Sys.sleep(0.1)
toc_mem(log = TRUE, quiet = TRUE)

mem_log()
}
\seealso{
\code{\link{mem_clearlog}} to clear the log, \code{\link{toc_mem}} with \code{log = TRUE}
}
