# Memory Query Functions
#
# This module provides functions for querying process memory information
# using the ps package. It wraps ps functions with error handling and
# provides a consistent interface for the rest of the package.


#' Get memory information for a process
#'
#' Queries the operating system for memory usage statistics of a given process.
#' On Windows, RSS corresponds to the Working Set (what Task Manager shows).
#'
#' @param pid Process ID to query. If NULL, queries the current R process.
#' @return A list with components:
#'   \item{rss}{Resident Set Size in bytes (physical RAM in use)}
#'   \item{vms}{Virtual Memory Size in bytes}
#'   \item{timestamp}{POSIXct timestamp when the measurement was taken}
#'   \item{pid}{Process ID that was queried}
#'   \item{success}{Logical indicating if the query succeeded}
#' @noRd
get_memory_info <- function(pid = NULL) {
  timestamp <- Sys.time()
  
  handle <- tryCatch(
    {
      if (is.null(pid)) {
        ps::ps_handle()
      } else {
        ps::ps_handle(pid)
      }
    },
    error = function(e) NULL
  )
  
  if (is.null(handle)) {
    return(list(
      rss = NA_real_,
      vms = NA_real_,
      timestamp = timestamp,
      pid = pid %||% NA_integer_,
      success = FALSE
    ))
  }
  
  mem <- tryCatch(
    ps::ps_memory_info(handle),
    error = function(e) NULL
  )
  
  if (is.null(mem)) {
    return(list(
      rss = NA_real_,
      vms = NA_real_,
      timestamp = timestamp,
      pid = tryCatch(ps::ps_pid(handle), error = function(e) NA_integer_),
      success = FALSE
    ))
  }
  
  list(
    rss = as.numeric(mem["rss"]),
    vms = as.numeric(mem["vms"]),
    timestamp = timestamp,
    pid = ps::ps_pid(handle),
    success = TRUE
  )
}


#' Check if memory queries are available on this system
#'
#' Tests whether the ps package can successfully query memory information.
#' This is used for graceful degradation on systems where ps is unavailable
#' or restricted.
#'
#' @return Logical TRUE if memory queries work
#' @noRd
memory_available <- function() {
  result <- get_memory_info()
  result$success
}


#' Null coalescing operator
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
