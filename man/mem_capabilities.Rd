\name{mem_capabilities}
\alias{mem_capabilities}
\title{Check memtoc capabilities}
\usage{
mem_capabilities()
}
\value{
A named logical vector with elements:
\describe{
  \item{memory_queries}{Can query process memory via ps package}
  \item{background_polling}{Can spawn background processes via callr}
}
}
\description{
Tests which features are available on the current system. This is useful
for understanding why certain features might be disabled.
}
\examples{
mem_capabilities()
}
