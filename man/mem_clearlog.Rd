\name{mem_clearlog}
\alias{mem_clearlog}
\title{Clear the memory tracking log}
\usage{
mem_clearlog()
}
\value{
Invisibly returns \code{NULL}.
}
\description{
Removes all entries from the memtoc log. This does not affect the
tracking stack (active tic_mem/toc_mem blocks).
}
\examples{
tic_mem("test")
toc_mem(log = TRUE, quiet = TRUE)

nrow(mem_log())
#> 1

mem_clearlog()
nrow(mem_log())
#> 0
}
\seealso{
\code{\link{mem_log}} to retrieve the log before clearing
}
