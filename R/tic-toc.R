# Core tic_mem / toc_mem Functions
#
# This module provides the main user-facing API for memory tracking.
# The design mirrors the tictoc package's tic/toc functions.
#
# MVP 2 adds background polling for continuous memory trajectory capture.
# MVP 3 adds parallel worker monitoring (future package integration).


#' Start memory tracking
#'
#' Begins a memory tracking block. Call [toc_mem()] to end the block and
#' see the results. Multiple calls to `tic_mem()` can be nested, and each
#' `toc_mem()` will match with the most recent unmatched `tic_mem()`.
#'
#' @param msg Optional character string label for this tracking block.
#'   This label is displayed in the output from [toc_mem()] and stored
#'   in the log if `log = TRUE` is passed to `toc_mem()`.
#' @param quiet Logical. If `TRUE` (default), no message is printed when
#'   tracking starts. Set to `FALSE` to see a startup message.
#' @param interval Numeric. Polling interval in seconds for background
#'   monitoring. Set to `NULL` or `0` to disable background polling and
#'   use snapshot-only mode (faster but only captures start/end memory).
#'   Default is `1` second.
#' @param persist Logical. If `TRUE` (default), periodically saves samples
#'   to disk for crash recovery. Set to `FALSE` to disable persistence
#'   (slightly faster but no recovery if R crashes).
#' @param workers Worker specification for parallel monitoring:
#'   \itemize{
#'     \item `"auto"` (default): Automatically detect future workers and child processes
#'     \item `"none"`: Only monitor the main R process
#'     \item `"children"`: Monitor main process and all child processes
#'     \item Integer vector: Explicit list of PIDs to monitor
#'   }
#'
#' @return Invisibly returns the timestamp when tracking started.
#'
#' @details
#' When `interval` is set (default), a background R process is spawned to
#' continuously sample memory usage. This captures the true peak memory
#' even for short-lived allocations. The background process adds minimal
#' overhead (typically less than 1% CPU).
#'
#' For very short operations (under 1 second), consider using `interval = NULL`
#' to avoid the background process startup overhead (~200ms).
#'
#' ## Parallel Worker Monitoring
#'
#' When using `future` for parallel processing, memtoc can automatically
#' detect and monitor worker processes. Set `workers = "auto"` to enable
#' this feature. The trajectory will include memory samples from all
#' workers, and the result will show aggregate statistics.
#'
#' @seealso [toc_mem()] to stop tracking, [mem_log()] to retrieve logged results,
#'   [mem_recover()] to recover data after a crash, [mem_parallel_info()] to
#'   check parallel backend status
#'
#' @export
#'
#' @examples
#' # Basic usage with background polling (default)
#' tic_mem("my operation")
#' Sys.sleep(0.5)
#' x <- numeric(1e6)
#' toc_mem()
#'
#' # Snapshot-only mode (no background process)
#' tic_mem("quick op", interval = NULL)
#' y <- 1:100
#' toc_mem()
#'
#' # Monitor with parallel workers (if future is set up)
#' # future::plan(future::multisession, workers = 2)
#' # tic_mem("parallel job", workers = "auto")
#' # result <- future.apply::future_lapply(1:10, function(x) rnorm(1e6))
#' # toc_mem()
#'
#' # Nested tracking
#' tic_mem("outer")
#'   tic_mem("inner", interval = NULL)
#'   Sys.sleep(0.1)
#'   toc_mem()
#' toc_mem()
tic_mem <- function(msg = NULL, quiet = TRUE, interval = 1, persist = TRUE,
                    workers = "auto") {
  # Capture memory state at start
  mem <- get_memory_info()
  
  if (!mem$success && !quiet) {
    cli::cli_warn(
      "Memory query failed. Results may be incomplete.",
      .frequency = "once",
      .frequency_id = "memtoc_memory_warning"
    )
  }
  
  # Resolve which PIDs to monitor
  pids_to_monitor <- resolve_worker_pids(workers, include_main = TRUE)
  n_workers <- length(pids_to_monitor) - 1L  # Exclude main process from count
  
  # Determine if we should use background monitoring
  use_monitor <- !is.null(interval) && interval > 0
  monitor <- NULL
  log_path <- NULL
  
  if (use_monitor) {
    # Set up persistence path
    log_path <- if (persist) default_log_path() else NULL
    
    # Start background monitor with all PIDs
    monitor <- tryCatch(
      start_monitor(
        pids = pids_to_monitor,
        interval = interval,
        log_path = log_path
      ),
      error = function(e) {
        if (!quiet) {
          cli::cli_warn(
            c(
              "Background monitor failed to start: {e$message}",
              "i" = "Falling back to snapshot-only mode."
            )
          )
        }
        NULL
      }
    )
  }
  
  # Create stack entry
  entry <- list(
    msg = msg,
    tic_time = proc.time()[["elapsed"]],
    tic_timestamp = Sys.time(),
    tic_mem = mem,
    depth = stack_depth() + 1L,
    monitor = monitor,
    log_path = log_path,
    interval = interval,
    pids_monitored = pids_to_monitor,
    n_workers = n_workers,
    workers_spec = workers
  )
  
  # Push to stack
  stack_push(entry)
  
  # Optional startup message
  if (!quiet) {
    mode_str <- if (use_monitor && !is.null(monitor)) {
      paste0("polling every ", interval, "s")
    } else {
      "snapshot mode"
    }
    
    workers_str <- if (n_workers > 0) {
      paste0(", ", n_workers, " worker", if (n_workers > 1) "s" else "")
    } else {
      ""
    }
    
    if (!is.null(msg)) {
      cli::cli_alert_info("Starting: {msg} ({mode_str}{workers_str})")
    } else {
      cli::cli_alert_info("Starting memory tracking ({mode_str}{workers_str})")
    }
  }
  
  invisible(mem$timestamp)
}


#' Stop memory tracking and report results
#'
#' Ends a memory tracking block started by [tic_mem()] and reports the results.
#' By default, prints a summary message showing peak memory, current memory,
#' and elapsed time.
#'
#' @param log Logical. If `TRUE`, stores the result in an internal log that
#'   can be retrieved later with [mem_log()]. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses the output message.
#'   Default is `FALSE` (message is shown).
#'
#' @return Invisibly returns a `memtoc_result` object (a list) containing:
#'   \item{msg}{The label passed to `tic_mem()`, or NULL}
#'   \item{mem_start}{Memory (RSS) in bytes at start (main process)}
#'   \item{mem_end}{Memory (RSS) in bytes at end (main process)}
#'   \item{mem_peak}{Peak memory during the interval (total across all processes)}
#'   \item{mem_change}{Change in memory (end - start) in bytes}
#'   \item{elapsed}{Elapsed time in seconds}
#'   \item{tic_timestamp}{POSIXct timestamp when tic_mem() was called}
#'   \item{toc_timestamp}{POSIXct timestamp when toc_mem() was called}
#'   \item{trajectory}{Data frame of memory samples (if background polling was used)}
#'   \item{n_samples}{Number of samples collected}
#'   \item{n_workers}{Number of worker processes monitored (0 if main process only)}
#'   \item{worker_stats}{Data frame with per-worker peak memory (if workers monitored)}
#'
#' @seealso [tic_mem()] to start tracking, [mem_log()] to retrieve logged results
#'
#' @export
#'
#' @examples
#' tic_mem("example")
#' x <- rnorm(1e6)
#' Sys.sleep(1)
#' result <- toc_mem()
#'
#' # Access result components
#' result$elapsed
#' result$mem_peak
#' result$n_samples
#'
#' # View the full trajectory (if available)
#' if (!is.null(result$trajectory)) {
#'   head(result$trajectory)
#' }
#'
#' # View per-worker stats (if workers were monitored)
#' if (!is.null(result$worker_stats)) {
#'   result$worker_stats
#' }
toc_mem <- function(log = FALSE, quiet = FALSE) {
  # Capture end state immediately
  toc_time <- proc.time()[["elapsed"]]
  toc_timestamp <- Sys.time()
  toc_mem_info <- get_memory_info()
  
  # Pop matching tic_mem entry
  entry <- stack_pop()
  
  # Stop the background monitor and get trajectory
  trajectory <- NULL
  if (!is.null(entry$monitor)) {
    trajectory <- stop_monitor(entry$monitor, log_path = entry$log_path)
  }
  
  # Clean up persistence file (after we've read from it)
  if (!is.null(entry$log_path)) {
    cleanup_persistence(entry$log_path)
  }
  
  # Calculate metrics
  elapsed <- toc_time - entry$tic_time
  mem_start <- entry$tic_mem$rss
  mem_end <- toc_mem_info$rss
  main_pid <- Sys.getpid()
  
  # Initialize variables
  sys_peak_percent <- NA_real_
  sys_total <- NA_real_
  cpu_user <- NA_real_
  cpu_system <- NA_real_
  worker_stats <- NULL
  n_workers <- if (!is.null(entry$n_workers)) entry$n_workers else 0L
  
  # Determine peak memory and extract system info from trajectory
  if (!is.null(trajectory) && is.data.frame(trajectory) && nrow(trajectory) > 0) {
    n_samples <- nrow(trajectory)
    
    # Calculate per-worker statistics if multiple PIDs
    unique_pids <- unique(trajectory$pid)
    
    if (length(unique_pids) > 1) {
      # Multiple processes monitored
      worker_stats <- calculate_worker_stats(trajectory, main_pid)
      
      # Total peak = max of sum of RSS at each timestamp
      # Group by timestamp and sum RSS, then take max
      total_by_time <- aggregate(rss ~ timestamp, data = trajectory, FUN = sum)
      mem_peak <- max(total_by_time$rss, na.rm = TRUE)
    } else {
      # Single process
      mem_peak <- max(trajectory$rss, na.rm = TRUE)
    }
    
    # Extract system memory info if available
    if ("sys_percent" %in% names(trajectory)) {
      sys_peak_percent <- max(trajectory$sys_percent, na.rm = TRUE)
      if (is.infinite(sys_peak_percent)) sys_peak_percent <- NA_real_
    }
    if ("sys_total" %in% names(trajectory)) {
      sys_total <- trajectory$sys_total[1]
    }
    
    # Extract CPU times for main process only
    main_traj <- trajectory[trajectory$pid == main_pid, ]
    if ("cpu_user" %in% names(main_traj) && nrow(main_traj) > 1) {
      cpu_user <- main_traj$cpu_user[nrow(main_traj)] - main_traj$cpu_user[1]
      cpu_system <- main_traj$cpu_system[nrow(main_traj)] - main_traj$cpu_system[1]
      if (is.na(cpu_user) || cpu_user < 0) cpu_user <- NA_real_
      if (is.na(cpu_system) || cpu_system < 0) cpu_system <- NA_real_
    }
  } else {
    # Fallback to snapshot mode
    mem_peak <- max(c(mem_start, mem_end), na.rm = TRUE)
    n_samples <- 0L
  }
  
  if (is.infinite(mem_peak) || is.na(mem_peak)) {
    mem_peak <- NA_real_
  }
  
  mem_change <- if (!is.na(mem_end) && !is.na(mem_start)) {
    mem_end - mem_start
  } else {
    NA_real_
  }
  
  # Build result object
  result <- structure(
    list(
      msg = entry$msg,
      mem_start = mem_start,
      mem_end = mem_end,
      mem_peak = mem_peak,
      mem_change = mem_change,
      elapsed = elapsed,
      tic_timestamp = entry$tic_timestamp,
      toc_timestamp = toc_timestamp,
      trajectory = trajectory,
      n_samples = n_samples,
      n_workers = n_workers,
      worker_stats = worker_stats,
      sys_peak_percent = sys_peak_percent,
      sys_total = sys_total,
      cpu_user = cpu_user,
      cpu_system = cpu_system
    ),
    class = "memtoc_result"
  )
  
  # Add to log if requested
  if (log) {
    log_push(result)
  }
  
  # Print summary if not quiet
  if (!quiet) {
    print_toc_message(result)
  }
  
  invisible(result)
}


#' Calculate per-worker memory statistics
#' @param trajectory Data frame with pid and rss columns
#' @param main_pid The main R process PID
#' @return Data frame with per-worker stats
#' @noRd
calculate_worker_stats <- function(trajectory, main_pid) {
  if (is.null(trajectory) || nrow(trajectory) == 0) {
    return(NULL)
  }
  
  unique_pids <- unique(trajectory$pid)
  
  stats_list <- lapply(unique_pids, function(pid) {
    pid_data <- trajectory[trajectory$pid == pid, ]
    
    data.frame(
      pid = pid,
      is_main = pid == main_pid,
      mem_peak = max(pid_data$rss, na.rm = TRUE),
      mem_mean = mean(pid_data$rss, na.rm = TRUE),
      mem_min = min(pid_data$rss, na.rm = TRUE),
      n_samples = nrow(pid_data),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, stats_list)
}


#' Print the toc_mem summary message
#' @param result A memtoc_result object
#' @noRd
print_toc_message <- function(result) {
  # Build label prefix
  label <- if (!is.null(result$msg) && nchar(result$msg) > 0) {
    paste0(result$msg, ": ")
  } else {
    ""
  }
  
  # Format components
  peak_str <- format_bytes(result$mem_peak)
  current_str <- format_bytes(result$mem_end)
  time_str <- format_duration(result$elapsed)
  
  # Add sample count if available
  samples_str <- if (result$n_samples > 0) {
    paste0(" | ", result$n_samples, " sample", if (result$n_samples != 1) "s" else "")
  } else {
    ""
  }
  
  # Add worker count if available
  workers_str <- if (!is.null(result$n_workers) && result$n_workers > 0) {
    paste0(" | ", result$n_workers, " worker", if (result$n_workers != 1) "s" else "")
  } else {
    ""
  }
  
  # Use cli for nice formatting
  cli::cli_alert_success(
    "{label}{peak_str} peak | {current_str} current | {time_str}{samples_str}{workers_str}"
  )
  
  # Warn if system memory was high
  if (!is.na(result$sys_peak_percent) && result$sys_peak_percent > 80) {
    if (result$sys_peak_percent > 95) {
      cli::cli_alert_danger("System RAM critical: {round(result$sys_peak_percent, 1)}% used")
    } else {
      cli::cli_alert_warning("System RAM high: {round(result$sys_peak_percent, 1)}% used")
    }
  }
}


#' Print method for memtoc_result objects
#'
#' @param x A memtoc_result object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns x
#' @export
print.memtoc_result <- function(x, ...) {
  cli::cli_h3("memtoc result")
  
  if (!is.null(x$msg)) {
    cli::cli_text("Label: {.val {x$msg}}")
  }
  
  cli::cli_bullets(c(
    "*" = "Memory at start: {format_bytes(x$mem_start)}",
    "*" = "Memory at end: {format_bytes(x$mem_end)}",
    "*" = "Peak memory (total): {format_bytes(x$mem_peak)}",
    "*" = "Memory change: {format_mem_change(x$mem_change)}",
    "*" = "Elapsed time: {format_duration(x$elapsed)}",
    "*" = "Samples collected: {x$n_samples}"
  ))
  
  # Show worker info if available
  if (!is.null(x$n_workers) && x$n_workers > 0) {
    cli::cli_text("")
    cli::cli_text("{.strong Parallel workers:} {x$n_workers}")
    
    if (!is.null(x$worker_stats) && nrow(x$worker_stats) > 0) {
      for (i in seq_len(nrow(x$worker_stats))) {
        row <- x$worker_stats[i, ]
        label <- if (row$is_main) "main" else paste0("worker")
        cli::cli_bullets(c(
          "*" = "PID {row$pid} ({label}): {format_bytes(row$mem_peak)} peak"
        ))
      }
    }
  }
  
  # Show system memory info if available
  if (!is.na(x$sys_peak_percent)) {
    cli::cli_text("")
    cli::cli_text("{.strong System context:}")
    sys_total_str <- if (!is.na(x$sys_total)) format_bytes(x$sys_total) else "NA"
    cli::cli_bullets(c(
      "*" = "System RAM total: {sys_total_str}",
      "*" = "System RAM peak usage: {round(x$sys_peak_percent, 1)}%"
    ))
  }
  
  # Show CPU times if available
  if (!is.na(x$cpu_user)) {
    cli::cli_text("")
    cli::cli_text("{.strong CPU time:}")
    cli::cli_bullets(c(
      "*" = "User: {round(x$cpu_user, 2)} sec",
      "*" = "System: {round(x$cpu_system, 2)} sec"
    ))
  }
  
  if (!is.null(x$trajectory) && nrow(x$trajectory) > 0) {
    cli::cli_text("")
    cli::cli_text("Trajectory data available in {.code $trajectory}")
  }
  
  if (!is.null(x$worker_stats) && nrow(x$worker_stats) > 0) {
    cli::cli_text("Per-worker stats available in {.code $worker_stats}")
  }
  
  invisible(x)
}


#' Clear the memtoc stack
#'
#' Removes all entries from the tracking stack. This is useful if an error
#' occurred before `toc_mem()` could be called, leaving orphaned entries
#' on the stack.
#'
#' Also stops any running background monitors associated with orphaned entries.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#'
#' @examples
#' tic_mem("will be cleared")
#' mem_clear()
#' # Stack is now empty
mem_clear <- function() {
  # Get current stack depth
  n <- stack_depth()
  
  if (n > 0) {
    # Attempt to stop any running monitors
    while (!stack_is_empty()) {
      entry <- tryCatch(stack_pop(), error = function(e) NULL)
      if (!is.null(entry) && !is.null(entry$monitor)) {
        tryCatch(
          {
            if (entry$monitor$is_alive()) {
              entry$monitor$kill()
            }
          },
          error = function(e) NULL
        )
      }
      # Clean up persistence files
      if (!is.null(entry) && !is.null(entry$log_path)) {
        cleanup_persistence(entry$log_path)
      }
    }
    
    cli::cli_alert_info("Cleared {n} item{?s} from the memtoc stack.")
  }
  
  invisible(NULL)
}


#' Check memtoc capabilities
#'
#' Tests which features are available on the current system. This is useful
#' for understanding why certain features might be disabled.
#'
#' @return A named logical vector with elements:
#'   \item{memory_queries}{Can query process memory via ps package}
#'   \item{background_polling}{Can spawn background processes via callr}
#'
#' @export
#'
#' @examples
#' mem_capabilities()
mem_capabilities <- function() {
  c(
    memory_queries = memory_available(),
    background_polling = monitor_available()
  )
}
