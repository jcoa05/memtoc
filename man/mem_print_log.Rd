\name{mem_print_log}
\alias{mem_print_log}
\title{Print formatted log output}
\usage{
mem_print_log()
}
\value{
Invisibly returns a character vector of formatted log lines.
}
\description{
Displays the memory tracking log in a human-readable format, similar to
how tictoc's tic.log() output can be printed with writeLines().
}
\examples{
mem_clearlog()

tic_mem("step 1")
Sys.sleep(0.1)
toc_mem(log = TRUE, quiet = TRUE)

tic_mem("step 2")
Sys.sleep(0.2)
toc_mem(log = TRUE, quiet = TRUE)

mem_print_log()
}
