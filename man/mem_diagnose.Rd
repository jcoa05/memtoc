\name{mem_diagnose}
\alias{mem_diagnose}
\title{Diagnose memtoc capabilities}
\usage{
mem_diagnose()
}
\value{
Invisibly returns a list with diagnostic results including:
\describe{
  \item{ps_ok}{Whether ps memory queries work}
  \item{pids_ok}{Whether process listing works}
  \item{sys_mem_ok}{Whether system memory queries work}
  \item{callr_loaded}{Whether callr package is installed}
  \item{callr_works}{Whether background processes can be spawned}
  \item{all_ok}{Whether all checks passed}
}
}
\description{
Runs detailed diagnostics to identify issues with background process
spawning. Useful for troubleshooting when \code{mem_capabilities()} shows
\code{background_polling = FALSE}.
}
\details{
This function tests each component of memtoc's functionality and provides
detailed error messages and suggestions for fixing issues, particularly
on Windows where Rtools may be required for background process spawning.
}
\examples{
mem_diagnose()
}
\seealso{
\code{\link{mem_capabilities}} for a quick capability check
}
