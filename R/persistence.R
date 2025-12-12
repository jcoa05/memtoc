# Persistence Module
#
# This module provides functions for atomic file I/O, ensuring that
# memory samples are safely persisted even if the R process crashes.
# Uses atomic rename operations to prevent corrupt partial writes.


#' Save data atomically (crash-safe)
#'
#' Writes data to a temporary file first, then atomically renames it
#' to the final destination. This ensures the file is never in a
#' partially-written state, even if the process is killed mid-write.
#'
#' @param data Data to save (will be serialized with saveRDS)
#' @param path Final destination path
#'
#' @return Invisible TRUE on success, FALSE on failure
#' @noRd
save_atomic <- function(data, path) {
  # Ensure directory exists
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Write to temporary file in same directory (required for atomic rename)
  temp <- tempfile(tmpdir = dir, fileext = ".rds.tmp")
  
  success <- tryCatch(
    {
      saveRDS(data, temp)
      
      # Atomic rename (works on both NTFS and POSIX)
      file.rename(temp, path)
      TRUE
    },
    error = function(e) {
      # Clean up temp file on error
      unlink(temp, force = TRUE)
      FALSE
    }
  )
  
  invisible(success)
}


#' Load data safely with fallback
#'
#' Attempts to load an RDS file, returning a default value if the
#' file doesn't exist or is corrupted.
#'
#' @param path Path to RDS file
#' @param default Value to return if file is missing or corrupt
#'
#' @return Loaded data or default value
#' @noRd
load_safe <- function(path, default = NULL) {
  if (!file.exists(path)) {
    return(default)
  }
  
  tryCatch(
    readRDS(path),
    error = function(e) default
  )
}


#' Get the default log path for a process
#'
#' Returns a standardized path in the temp directory for storing
#' memory monitoring data. Each process gets its own file based on PID.
#'
#' @param pid Process ID (defaults to current process)
#'
#' @return Character path to the log file
#' @noRd
default_log_path <- function(pid = Sys.getpid()) {
  memtoc_dir <- file.path(tempdir(), "memtoc")
  file.path(memtoc_dir, paste0("memtoc_", pid, ".rds"))
}


#' Clean up a persistence file
#'
#' Removes a persistence file after successful completion.
#'
#' @param path Path to the file to remove
#'
#' @return Invisible NULL
#' @noRd
cleanup_persistence <- function(path) {
  if (!is.null(path) && file.exists(path)) {
    unlink(path, force = TRUE)
  }
  invisible(NULL)
}


#' Recover data from a crashed monitoring session
#'
#' Attempts to recover memory samples from a previous session that
#' crashed or was interrupted before toc_mem() was called. The monitor
#' periodically saves checkpoints to disk, so some data may be recoverable.
#'
#' @param pid Process ID of the crashed R session. If NULL, lists all
#'   available recovery files.
#' @param path Direct path to a recovery file. Overrides pid if provided.
#'
#' @return If pid or path is provided, returns a data frame of recovered
#'   samples or NULL if not found. If both are NULL, returns a list of
#'   available recovery files with their metadata.
#'
#' @export
#'
#' @examples
#' # List available recovery files
#' mem_recover()
#'
#' # Recover from a specific PID
#' # mem_recover(pid = 12345)
#'
#' # Recover from a specific file
#' # mem_recover(path = "/path/to/memtoc_12345.rds")
mem_recover <- function(pid = NULL, path = NULL) {
  # Direct path recovery
  if (!is.null(path)) {
    if (!file.exists(path)) {
      cli::cli_alert_warning("Recovery file not found: {.file {path}}")
      return(NULL)
    }
    
    data <- load_safe(path)
    if (is.null(data)) {
      cli::cli_alert_warning("Could not read recovery file: {.file {path}}")
      return(NULL)
    }
    
    cli::cli_alert_success("Recovered {nrow(data)} sample{?s} from {.file {path}}")
    return(data)
  }
  
  # PID-based recovery
  if (!is.null(pid)) {
    path <- default_log_path(pid)
    
    if (!file.exists(path)) {
      cli::cli_alert_warning("No recovery file found for PID {pid}")
      return(NULL)
    }
    
    data <- load_safe(path)
    if (is.null(data)) {
      cli::cli_alert_warning("Could not read recovery file for PID {pid}")
      return(NULL)
    }
    
    cli::cli_alert_success("Recovered {nrow(data)} sample{?s} for PID {pid}")
    return(data)
  }
  
  # List all recovery files
  recovery_dir <- file.path(tempdir(), "memtoc")
  
  if (!dir.exists(recovery_dir)) {
    cli::cli_alert_info("No recovery directory found.")
    return(invisible(list()))
  }
  
  files <- list.files(recovery_dir, pattern = "^memtoc_.*\\.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    cli::cli_alert_info("No recovery files found.")
    return(invisible(list()))
  }
  
  # Build info about each file
  file_info <- lapply(files, function(f) {
    info <- file.info(f)
    pid_match <- regmatches(f, regexpr("\\d+", f))
    pid <- if (length(pid_match) > 0) as.integer(pid_match) else NA_integer_
    
    # Try to peek at the data
    data <- load_safe(f)
    n_samples <- if (is.data.frame(data)) nrow(data) else NA_integer_
    
    list(
      path = f,
      pid = pid,
      size = info$size,
      modified = info$mtime,
      n_samples = n_samples
    )
  })
  
  cli::cli_alert_info("Found {length(files)} recovery file{?s}:")
  
  for (info in file_info) {
    size_str <- format_bytes(info$size)
    samples_str <- if (!is.na(info$n_samples)) paste0(info$n_samples, " samples") else "unknown"
    cli::cli_bullets(c(
      "*" = "PID {info$pid}: {size_str}, {samples_str}, modified {info$modified}"
    ))
  }
  
  invisible(file_info)
}


#' Clean up all recovery files
#'
#' Removes all memtoc recovery files from the temp directory. Use with
#' caution as this deletes potentially recoverable data.
#'
#' @param force If FALSE (default), asks for confirmation. If TRUE,
#'   deletes without confirmation.
#'
#' @return Invisible NULL
#' @noRd
cleanup_all_recovery <- function(force = FALSE) {
  recovery_dir <- file.path(tempdir(), "memtoc")
  
  if (!dir.exists(recovery_dir)) {
    return(invisible(NULL))
  }
  
  files <- list.files(recovery_dir, pattern = "^memtoc_.*\\.rds$", full.names = TRUE)
  
  if (length(files) == 0) {
    return(invisible(NULL))
  }
  
  if (!force) {
    cli::cli_alert_warning(
      "This will delete {length(files)} recovery file{?s}. Use force = TRUE to confirm."
    )
    return(invisible(NULL))
  }
  
  unlink(files, force = TRUE)
  cli::cli_alert_success("Deleted {length(files)} recovery file{?s}.")
  
  invisible(NULL)
}
