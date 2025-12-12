\name{mem_clear}
\alias{mem_clear}
\title{Clear the memtoc stack}
\usage{
mem_clear()
}
\value{
Invisibly returns \code{NULL}.
}
\description{
Removes all entries from the tracking stack. This is useful if an error
occurred before \code{toc_mem()} could be called, leaving orphaned entries
on the stack.
}
\examples{
tic_mem("will be cleared")
mem_clear()
# Stack is now empty
}
