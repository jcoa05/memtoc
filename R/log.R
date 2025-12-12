# Logging Functions
#
# This module provides functions for managing the memtoc log, which stores
# results from toc_mem() calls when log = TRUE is specified.


#' Retrieve the memory tracking log
#'
#' Returns a data frame containing all logged memory tracking results.
#' Results are added to the log when `toc_mem(log = TRUE)` is called.
#'
#' @param format Character string specifying the output format.
#'   \describe{
#'     \item{"data.frame"}{(Default) Returns a data frame with one row per logged result}
#'     \item{"list"}{Returns the raw list of memtoc_result objects}
#'   }
#'
#' @return A data frame (default) or list containing logged results. The data
#'   frame has columns:
#'   \describe{
#'     \item{msg}{Label from tic_mem(), or NA if none provided}
#'     \item{mem_start}{Memory (RSS) in bytes at start}
#'     \item{mem_end}{Memory (RSS) in bytes at end}
#'     \item{mem_peak}{Peak memory in bytes}
#'     \item{mem_change}{Change in memory (bytes)}
#'     \item{elapsed}{Elapsed time in seconds}
#'     \item{tic_timestamp}{When tic_mem() was called}
#'     \item{toc_timestamp}{When toc_mem() was called}
#'   }
#'
#' @seealso [mem_clearlog()] to clear the log, [toc_mem()] with `log = TRUE`
#'
#' @export
#'
#' @examples
#' mem_clearlog()
#'
#' tic_mem("step 1")
#' Sys.sleep(0.1)
#' toc_mem(log = TRUE, quiet = TRUE)
#'
#' tic_mem("step 2")
#' Sys.sleep(0.1)
#' toc_mem(log = TRUE, quiet = TRUE)
#'
#' mem_log()
mem_log <- function(format = c("data.frame", "list")) {
  format <- match.arg(format)
  
  entries <- log_get()
  
  if (length(entries) == 0) {
    if (format == "list") {
      return(list())
    }
    # Return empty data frame with correct structure
    return(data.frame(
      msg = character(0),
      mem_start = numeric(0),
      mem_end = numeric(0),
      mem_peak = numeric(0),
      mem_change = numeric(0),
      elapsed = numeric(0),
      tic_timestamp = as.POSIXct(character(0)),
      toc_timestamp = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }
  
  if (format == "list") {
    return(entries)
  }
  
  # Convert list of results to data frame
  data.frame(
    msg = vapply(entries, function(x) x$msg %||% NA_character_, character(1)),
    mem_start = vapply(entries, function(x) x$mem_start %||% NA_real_, numeric(1)),
    mem_end = vapply(entries, function(x) x$mem_end %||% NA_real_, numeric(1)),
    mem_peak = vapply(entries, function(x) x$mem_peak %||% NA_real_, numeric(1)),
    mem_change = vapply(entries, function(x) x$mem_change %||% NA_real_, numeric(1)),
    elapsed = vapply(entries, function(x) x$elapsed %||% NA_real_, numeric(1)),
    tic_timestamp = do.call(c, lapply(entries, function(x) x$tic_timestamp)),
    toc_timestamp = do.call(c, lapply(entries, function(x) x$toc_timestamp)),
    stringsAsFactors = FALSE
  )
}


#' Clear the memory tracking log
#'
#' Removes all entries from the memtoc log. This does not affect the
#' tracking stack (active tic_mem/toc_mem blocks).
#'
#' @return Invisibly returns `NULL`.
#'
#' @seealso [mem_log()] to retrieve the log before clearing
#'
#' @export
#'
#' @examples
#' tic_mem("test")
#' toc_mem(log = TRUE, quiet = TRUE)
#'
#' nrow(mem_log())
#' #> 1
#'
#' mem_clearlog()
#' nrow(mem_log())
#' #> 0
mem_clearlog <- function() {
  n <- log_length()
  log_clear()
  if (n > 0) {
    cli::cli_alert_info("Cleared {n} entr{?y/ies} from the memtoc log.")
  }
  invisible(NULL)
}


#' Print formatted log output
#'
#' Displays the memory tracking log in a human-readable format, similar to
#' how tictoc's tic.log() output can be printed with writeLines().
#'
#' @return Invisibly returns a character vector of formatted log lines.
#'
#' @export
#'
#' @examples
#' mem_clearlog()
#'
#' tic_mem("step 1")
#' Sys.sleep(0.1)
#' toc_mem(log = TRUE, quiet = TRUE)
#'
#' tic_mem("step 2")
#' Sys.sleep(0.2)
#' toc_mem(log = TRUE, quiet = TRUE)
#'
#' mem_print_log()
mem_print_log <- function() {
  entries <- log_get()
  
  if (length(entries) == 0) {
    cli::cli_alert_info("Log is empty.")
    return(invisible(character(0)))
  }
  
  lines <- vapply(entries, function(x) {
    label <- if (!is.null(x$msg)) paste0(x$msg, ": ") else ""
    sprintf(
      "%s%s peak | %s current | %s",
      label,
      format_bytes(x$mem_peak),
      format_bytes(x$mem_end),
      format_duration(x$elapsed)
    )
  }, character(1))
  
  writeLines(lines)
  invisible(lines)
}
