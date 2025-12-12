# Internal Stack Implementation for memtoc
# 
# This module provides a simple stack data structure used to track nested
# tic_mem/toc_mem calls. The stack is stored in a package environment to
# persist across function calls within a session.

# Package environment for storing state
# Using a dedicated environment prevents issues with global variable assignment
.memtoc_env <- new.env(parent = emptyenv())
.memtoc_env$stack <- list()
.memtoc_env$log <- list()


#' Push an item onto the memtoc stack
#' @param item A list containing tic_mem entry data
#' @return NULL (invisible)
#' @noRd
stack_push <- function(item) {
 .memtoc_env$stack <- c(.memtoc_env$stack, list(item))
  invisible(NULL)
}


#' Pop an item from the memtoc stack
#' @return The most recently pushed item
#' @noRd
stack_pop <- function() {
  n <- length(.memtoc_env$stack)
  
 if (n == 0L) {
    cli::cli_abort(
      c(
        "memtoc stack is empty.",
        "i" = "Did you forget to call {.fn tic_mem} first?",
        "i" = "Use {.fn mem_clear} to reset the stack if needed."
      ),
      call = NULL
    )
  }
  
  item <- .memtoc_env$stack[[n]]
  .memtoc_env$stack <- .memtoc_env$stack[-n]
  item
}


#' Get the current stack depth
#' @return Integer count of items on the stack
#' @noRd
stack_depth <- function() {
  length(.memtoc_env$stack)
}


#' Check if stack is empty
#' @return Logical TRUE if stack is empty
#' @noRd
stack_is_empty <- function() {
  length(.memtoc_env$stack) == 0L
}


#' Clear the entire stack
#' @return NULL (invisible)
#' @noRd
stack_clear <- function() {
  .memtoc_env$stack <- list()
  invisible(NULL)
}


#' Push a result to the log
#' @param result A memtoc_result object
#' @return NULL (invisible)
#' @noRd
log_push <- function(result) {
  .memtoc_env$log <- c(.memtoc_env$log, list(result))
  invisible(NULL)
}


#' Get all log entries
#' @return List of memtoc_result objects
#' @noRd
log_get <- function() {
  .memtoc_env$log
}


#' Clear the log
#' @return NULL (invisible)
#' @noRd
log_clear <- function() {
  .memtoc_env$log <- list()
  invisible(NULL)
}


#' Get log length
#' @return Integer count of log entries
#' @noRd
log_length <- function() {
  length(.memtoc_env$log)
}
