# Utility Functions
#
# Helper functions for formatting output and other common operations.


#' Format bytes into human-readable units
#'
#' Converts a byte count into a human-readable string with appropriate units
#' (B, KB, MB, GB, TB).
#'
#' @param bytes Numeric value in bytes
#' @param digits Number of decimal places to show
#' @return Character string with formatted value and unit
#' @noRd
#'
#' @examples
#' format_bytes(1024)
#' #> "1 KB"
#' format_bytes(1536000)
#' #> "1.5 MB"
format_bytes <- function(bytes, digits = 1) {
  if (is.na(bytes) || is.null(bytes)) {
    return("NA")
  }
  
  if (!is.numeric(bytes) || length(bytes) != 1) {
    return("NA")
  }
  
  if (bytes < 0) {
    sign <- "-"
    bytes <- abs(bytes)
  } else {
    sign <- ""
  }
  
  if (bytes == 0) {
    return("0 B")
  }
  
  units <- c("B", "KB", "MB", "GB", "TB", "PB")
  
  # Calculate the appropriate power of 1024
  power <- floor(log(bytes, 1024))
  power <- min(power, length(units) - 1)
  power <- max(power, 0)
  
  value <- bytes / (1024^power)
  
  paste0(sign, round(value, digits), " ", units[power + 1])
}


#' Format a time duration
#'
#' Converts seconds into a human-readable duration string.
#'
#' @param seconds Numeric value in seconds
#' @param digits Number of decimal places for sub-minute values
#' @return Character string with formatted duration
#' @noRd
format_duration <- function(seconds, digits = 2) {
  if (is.na(seconds) || is.null(seconds)) {
    return("NA")
  }
  
  if (seconds < 60) {
    return(paste0(round(seconds, digits), " sec"))
  }
  
  if (seconds < 3600) {
    mins <- floor(seconds / 60)
    secs <- round(seconds %% 60, 1)
    return(paste0(mins, " min ", secs, " sec"))
  }
  
  hours <- floor(seconds / 3600)
  mins <- floor((seconds %% 3600) / 60)
  return(paste0(hours, " hr ", mins, " min"))
}


#' Format memory change with sign
#'
#' @param bytes_change Change in bytes (can be negative)
#' @return Formatted string with + or - prefix
#' @noRd
format_mem_change <- function(bytes_change) {
  if (is.na(bytes_change)) {
    return("NA")
  }
  
  sign <- if (bytes_change >= 0) "+" else ""
  paste0(sign, format_bytes(bytes_change))
}
