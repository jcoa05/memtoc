#' @keywords internal
"_PACKAGE"
#' memtoc: Tictoc-Style Memory Usage Tracking
#'
#' @description
#' The memtoc package provides simple start/stop memory tracking functions
#' that can be nested, inspired by the tictoc package for timing. Track RAM
#' usage during code execution with support for logging and custom messages.
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{tic_mem}}: Start memory tracking
#'   \item \code{\link{toc_mem}}: Stop memory tracking and report
#'   \item \code{\link{mem_log}}: Retrieve logged memory measurements
#'   \item \code{\link{mem_clearlog}}: Clear the memory log
#'   \item \code{\link{mem_clear}}: Clear the tracking stack (useful after errors)
#' }
#'
#' @section Basic Usage:
#' \preformatted{
#' tic_mem("data loading")
#' data <- read.csv("large_file.csv")
#' toc_mem()
#' #> data loading: 142.3 MB peak | 89.1 MB current | 2.34 sec elapsed
#' }
#'
#' @section Nested Tracking:
#' \preformatted{
#' tic_mem("full pipeline")
#'   tic_mem("preprocessing")
#'   # ... code ...
#'   toc_mem()
#'
#'   tic_mem("modeling")
#'   # ... code ...
#'   toc_mem()
#' toc_mem()
#' }
#'
#' @docType package
#' @name memtoc-package
NULL
