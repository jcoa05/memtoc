\name{mem_recover}
\alias{mem_recover}
\title{Recover data from a crashed monitoring session}
\usage{
mem_recover(pid = NULL, path = NULL)
}
\arguments{
\item{pid}{Process ID of the crashed R session. If NULL, lists all
available recovery files.}

\item{path}{Direct path to a recovery file. Overrides pid if provided.}
}
\value{
If pid or path is provided, returns a data frame of recovered
samples or NULL if not found. If both are NULL, returns a list of
available recovery files with their metadata.
}
\description{
Attempts to recover memory samples from a previous session that
crashed or was interrupted before toc_mem() was called. The monitor
periodically saves checkpoints to disk, so some data may be recoverable.
}
\examples{
# List available recovery files
mem_recover()

# Recover from a specific PID
# mem_recover(pid = 12345)

# Recover from a specific file
# mem_recover(path = "/path/to/memtoc_12345.rds")
}
\seealso{
\code{\link{tic_mem}} for starting memory tracking with persistence enabled
}
