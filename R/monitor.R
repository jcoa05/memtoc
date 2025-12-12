# Background Monitor Module
#
# This module provides functions for running a background process that
# continuously samples memory usage. The monitor runs in a separate R
# process via callr::r_bg(), allowing it to sample while the main
# process is busy with computation.


#' Check if a process is alive
#'
#' Uses ps_pids() to check if a given PID is in the list of active processes.
#'
#' @param pid Process ID to check
#' @return Logical TRUE if process is alive
#' @noRd
pid_is_alive <- function(pid) {
  tryCatch(
    pid %in% ps::ps_pids(),
    error = function(e) FALSE
  )
}


#' Start a background memory monitor
#'
#' Launches a separate R process that periodically samples memory usage
#' for the specified process IDs. The monitor runs independently and
#' continues sampling until stopped or the parent process exits.
#'
#' @param pids Integer vector of process IDs to monitor. The first PID
#'   is assumed to be the parent process.
#' @param interval Numeric. Sampling interval in seconds. Default is 1.
#' @param log_path Character. Path for atomic persistence of samples.
#'   If NULL, no persistence is performed (samples only kept in memory).
#'
#' @return A callr process handle that can be used with stop_monitor().
#' @noRd
start_monitor <- function(pids, interval = 1, log_path = NULL) {
  # Validate inputs
  if (length(pids) == 0) {
    cli::cli_abort("At least one PID must be provided to monitor.")
  }
  
  pids <- as.integer(pids)
  interval <- as.numeric(interval)
  
  if (interval <= 0) {
    cli::cli_abort("Interval must be positive.")
  }
  
  # Launch background process
  # Note: We use package = FALSE and pass a self-contained function
  # because the memtoc package may not be properly installed
  callr::r_bg(
    func = function(pids, interval, log_path) {
      # Self-contained monitoring loop (no external package dependencies except ps)
      
      # Pre-allocate sample storage
      samples <- vector("list", 1000)
      sample_count <- 0L
      checkpoint_every <- 10L
      
      parent_pid <- pids[1]
      
      # Helper to check if parent is alive
      parent_alive <- function() {
        tryCatch(
          parent_pid %in% ps::ps_pids(),
          error = function(e) FALSE
        )
      }
      
      # Helper for atomic save
      save_atomic_internal <- function(data, path) {
        dir <- dirname(path)
        if (!dir.exists(dir)) {
          dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
        temp <- tempfile(tmpdir = dir, fileext = ".rds.tmp")
        tryCatch({
          saveRDS(data, temp)
          file.rename(temp, path)
        }, error = function(e) {
          unlink(temp, force = TRUE)
        })
      }
      
      # Main sampling loop
      while (parent_alive()) {
        timestamp <- Sys.time()
        
        # Get system memory info
        sys_mem <- tryCatch({
          sm <- ps::ps_system_memory()
          list(
            sys_total = as.numeric(sm[["total"]]),
            sys_avail = as.numeric(sm[["avail"]]),
            sys_percent = as.numeric(sm[["percent"]])
          )
        }, error = function(e) {
          list(sys_total = NA_real_, sys_avail = NA_real_, sys_percent = NA_real_)
        })
        
        # Sample all PIDs
        snapshot <- lapply(pids, function(pid) {
          tryCatch({
            handle <- ps::ps_handle(pid)
            mem <- ps::ps_memory_info(handle)
            
            # Get CPU times
            cpu <- tryCatch({
              ct <- ps::ps_cpu_times(handle)
              list(cpu_user = as.numeric(ct[["user"]]), cpu_system = as.numeric(ct[["system"]]))
            }, error = function(e) {
              list(cpu_user = NA_real_, cpu_system = NA_real_)
            })
            
            data.frame(
              timestamp = timestamp,
              pid = as.integer(pid),
              rss = as.numeric(mem["rss"]),
              cpu_user = cpu$cpu_user,
              cpu_system = cpu$cpu_system,
              sys_total = sys_mem$sys_total,
              sys_avail = sys_mem$sys_avail,
              sys_percent = sys_mem$sys_percent,
              stringsAsFactors = FALSE
            )
          }, error = function(e) NULL)
        })
        
        # Combine valid snapshots
        valid <- Filter(Negate(is.null), snapshot)
        
        if (length(valid) > 0) {
          sample_count <- sample_count + 1L
          
          # Grow storage if needed
          if (sample_count > length(samples)) {
            samples <- c(samples, vector("list", 1000))
          }
          
          samples[[sample_count]] <- do.call(rbind, valid)
          
          # Save checkpoint on every sample (since we'll be killed, not exit cleanly)
          if (!is.null(log_path)) {
            tryCatch({
              result <- do.call(rbind, samples[seq_len(sample_count)])
              save_atomic_internal(result, log_path)
            }, error = function(e) NULL)
          }
        }
        
        Sys.sleep(interval)
      }
      
      # Return final results
      if (sample_count > 0) {
        result <- do.call(rbind, samples[seq_len(sample_count)])
        if (!is.null(log_path)) {
          tryCatch(save_atomic_internal(result, log_path), error = function(e) NULL)
        }
        return(result)
      }
      
      # Return empty data frame
      data.frame(
        timestamp = as.POSIXct(character(0)),
        pid = integer(0),
        rss = numeric(0),
        cpu_user = numeric(0),
        cpu_system = numeric(0),
        sys_total = numeric(0),
        sys_avail = numeric(0),
        sys_percent = numeric(0),
        stringsAsFactors = FALSE
      )
    },
    args = list(
      pids = pids,
      interval = interval,
      log_path = log_path
    ),
    package = FALSE,  # Don't try to load memtoc package
    cleanup = TRUE
  )
}


#' The main monitoring loop (runs in background process)
#'
#' This function runs in the callr background process. It continuously
#' samples memory for all specified PIDs until the parent process exits.
#'
#' @param pids Integer vector of PIDs to monitor
#' @param interval Sampling interval in seconds
#' @param log_path Path for atomic persistence, or NULL
#'
#' @return Data frame of all collected samples
#' @noRd
monitor_loop <- function(pids, interval, log_path) {
  # Pre-allocate sample storage (grows as needed)
  samples <- vector("list", 1000)
  sample_count <- 0L
  checkpoint_every <- 10L
  
  parent_pid <- pids[1]
  start_time <- Sys.time()
  
  # Helper function to check if parent is alive (defined locally for background process)
  parent_alive <- function() {
    tryCatch(
      parent_pid %in% ps::ps_pids(),
      error = function(e) FALSE
    )
  }
  
  # Keep sampling while parent is alive
  while (parent_alive()) {
    timestamp <- Sys.time()
    
    # Get system-wide memory info
    sys_mem <- tryCatch(
      {
        sm <- ps::ps_system_memory()
        list(
          sys_total = as.numeric(sm[["total"]]),
          sys_avail = as.numeric(sm[["avail"]]),
          sys_percent = as.numeric(sm[["percent"]])
        )
      },
      error = function(e) list(sys_total = NA_real_, sys_avail = NA_real_, sys_percent = NA_real_)
    )
    
    # Sample all PIDs
    snapshot <- lapply(pids, function(pid) {
      tryCatch(
        {
          handle <- ps::ps_handle(pid)
          mem <- ps::ps_memory_info(handle)
          
          # Get CPU times for additional context
          cpu <- tryCatch(
            {
              ct <- ps::ps_cpu_times(handle)
              list(cpu_user = as.numeric(ct[["user"]]), cpu_system = as.numeric(ct[["system"]]))
            },
            error = function(e) list(cpu_user = NA_real_, cpu_system = NA_real_)
          )
          
          data.frame(
            timestamp = timestamp,
            pid = as.integer(pid),
            rss = as.numeric(mem["rss"]),
            cpu_user = cpu$cpu_user,
            cpu_system = cpu$cpu_system,
            sys_total = sys_mem$sys_total,
            sys_avail = sys_mem$sys_avail,
            sys_percent = sys_mem$sys_percent,
            stringsAsFactors = FALSE
          )
        },
        error = function(e) NULL
      )
    })
    
    # Combine valid snapshots
    valid <- Filter(Negate(is.null), snapshot)
    
    if (length(valid) > 0) {
      sample_count <- sample_count + 1L
      
      # Grow storage if needed
      if (sample_count > length(samples)) {
        samples <- c(samples, vector("list", 1000))
      }
      
      samples[[sample_count]] <- do.call(rbind, valid)
      
      # Periodic checkpoint for crash recovery
      if (!is.null(log_path) && sample_count %% checkpoint_every == 0L) {
        save_checkpoint(samples, sample_count, log_path)
      }
    }
    
    Sys.sleep(interval)
  }
  
  # Final save before exit
  if (sample_count > 0) {
    result <- do.call(rbind, samples[seq_len(sample_count)])
    
    if (!is.null(log_path)) {
      save_checkpoint_final(result, log_path)
    }
    
    return(result)
  }
  
  # Return empty data frame if no samples
  data.frame(
    timestamp = as.POSIXct(character(0)),
    pid = integer(0),
    rss = numeric(0),
    cpu_user = numeric(0),
    cpu_system = numeric(0),
    sys_total = numeric(0),
    sys_avail = numeric(0),
    sys_percent = numeric(0),
    stringsAsFactors = FALSE
  )
}


#' Save checkpoint during monitoring (called from background process)
#' @param samples List of sample data frames
#' @param count Number of valid samples
#' @param path File path for checkpoint
#' @noRd
save_checkpoint <- function(samples, count, path) {
  tryCatch(
    {
      result <- do.call(rbind, samples[seq_len(count)])
      save_atomic(result, path)
    },
    error = function(e) {
      # Silently ignore checkpoint errors
      NULL
    }
  )
}


#' Save final checkpoint when monitor exits
#' @param result Combined data frame of all samples
#' @param path File path for checkpoint
#' @noRd
save_checkpoint_final <- function(result, path) {
  tryCatch(
    save_atomic(result, path),
    error = function(e) NULL
  )
}


#' Stop the background monitor and retrieve results
#'
#' Terminates the background monitor process and retrieves the collected
#' memory samples. If the monitor has already exited (e.g., because the
#' sampling completed), retrieves the final results.
#'
#' @param process A callr process handle from start_monitor()
#' @param timeout Numeric. Maximum seconds to wait for results. Default is 5.
#' @param log_path Path to persistence file for crash recovery
#'
#' @return Data frame of memory samples with columns: timestamp, pid, rss,
#'   cpu_user, cpu_system, sys_total, sys_avail, sys_percent.
#'   Returns NULL if retrieval fails.
#' @noRd
stop_monitor <- function(process, timeout = 5, log_path = NULL) {
  if (is.null(process)) {
    return(NULL)
  }
  
  result <- NULL
  
  tryCatch(
    {
      # The monitor runs until parent dies, so we need to kill it
      # First, give it a moment to do one more sample
      Sys.sleep(0.2)
      
      if (process$is_alive()) {
        # Kill the process to stop monitoring
        process$kill()
        
        # Give it time to die
        Sys.sleep(0.1)
      }
      
      # Try to get the result (may fail if process was killed mid-execution)
      result <- tryCatch(
        process$get_result(),
        error = function(e) NULL
      )
    },
    error = function(e) {
      # Try to kill if error occurred
      tryCatch(process$kill(), error = function(e2) NULL)
    }
  )
  
  # If we got a valid result, return it
  if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
    return(result)
  }
  
  # Fallback: try to read from persistence file
  if (!is.null(log_path) && file.exists(log_path)) {
    result <- tryCatch(
      readRDS(log_path),
      error = function(e) NULL
    )
    
    if (!is.null(result) && is.data.frame(result) && nrow(result) > 0) {
      return(result)
    }
  }
  
  NULL
}


#' Check if background monitoring is available
#'
#' Tests whether callr can successfully spawn background processes.
#' This is used for graceful degradation on systems where background
#' processes are restricted.
#'
#' @return Logical TRUE if background monitoring works
#' @noRd
monitor_available <- function() {
  tryCatch(
    {
      # Try to spawn a simple background process
      # Use package = FALSE since memtoc may not be properly installed
      p <- callr::r_bg(function() TRUE, package = FALSE, cleanup = TRUE)
      # Use wait() then get_result() for compatibility
      p$wait(timeout = 10000)  # timeout in milliseconds
      result <- p$get_result()
      isTRUE(result)
    },
    error = function(e) FALSE
  )
}


#' Diagnose why background polling might not be working
#'
#' Runs detailed diagnostics to identify issues with background process
#' spawning. Useful for troubleshooting when `mem_capabilities()` shows
#' `background_polling = FALSE`.
#'
#' @return Invisibly returns a list with diagnostic results
#' @export
#'
#' @examples
#' mem_diagnose()
mem_diagnose <- function() {
  cli::cli_h2("memtoc Diagnostics")
  
  # Check 1: ps package
  cli::cli_h3("1. Memory Queries (ps package)")
  ps_ok <- tryCatch(
    {
      mem <- ps::ps_memory_info(ps::ps_handle())
      cli::cli_alert_success("ps::ps_memory_info() works")
      cli::cli_bullets(c(" " = "Current RSS: {format_bytes(mem['rss'])}"))
      TRUE
    },
    error = function(e) {
      cli::cli_alert_danger("ps::ps_memory_info() failed: {e$message}")
      FALSE
    }
  )
  
  # Check 2: ps_pids (our replacement for the hallucinated ps_pid_exists)
  cli::cli_h3("2. Process Listing (ps::ps_pids)")
  pids_ok <- tryCatch(
    {
      pids <- ps::ps_pids()
      my_pid <- Sys.getpid()
      found <- my_pid %in% pids
      if (found) {
        cli::cli_alert_success("ps::ps_pids() works, found {length(pids)} processes")
        cli::cli_bullets(c(" " = "Current PID ({my_pid}) found in list: {found}
"))
      } else {
        cli::cli_alert_warning("ps::ps_pids() works but current PID not found")
      }
      found
    },
    error = function(e) {
      cli::cli_alert_danger("ps::ps_pids() failed: {e$message}")
      FALSE
    }
  )
  
  # Check 3: System memory
  cli::cli_h3("3. System Memory (ps::ps_system_memory)")
  sys_mem_ok <- tryCatch(
    {
      sm <- ps::ps_system_memory()
      cli::cli_alert_success("ps::ps_system_memory() works")
      cli::cli_bullets(c(
        " " = "Total: {format_bytes(sm[['total']])}",
        " " = "Available: {format_bytes(sm[['avail']])}",
        " " = "Used: {round(sm[['percent']], 1)}%"
      ))
      TRUE
    },
    error = function(e) {
      cli::cli_alert_danger("ps::ps_system_memory() failed: {e$message}")
      FALSE
    }
  )
  
  # Check 4: callr availability
  cli::cli_h3("4. Background Processes (callr package)")
  callr_loaded <- tryCatch(
    {
      if (requireNamespace("callr", quietly = TRUE)) {
        cli::cli_alert_success("callr package is installed")
        TRUE
      } else {
        cli::cli_alert_danger("callr package is not installed")
        FALSE
      }
    },
    error = function(e) {
      cli::cli_alert_danger("Error checking callr: {e$message}")
      FALSE
    }
  )
  
  # Check 5: Actually spawn a background process
  callr_works <- FALSE
  if (callr_loaded) {
    cli::cli_h3("5. Background Process Spawn Test")
    callr_works <- tryCatch(
      {
        cli::cli_alert_info("Attempting to spawn background R process...")
        
        p <- callr::r_bg(
          function() {
            Sys.sleep(0.1)
            list(
              pid = Sys.getpid(),
              success = TRUE,
              r_version = R.version.string
            )
          },
          package = FALSE,  # Don't try to load calling package
          cleanup = TRUE
        )
        
        # Wait for process to complete (compatible with different callr versions)
        # Try wait() first, then get the result
        p$wait(timeout = 15000)  # timeout in milliseconds for wait()
        result <- p$get_result()
        
        if (isTRUE(result$success)) {
          cli::cli_alert_success("Background process spawned successfully!")
          cli::cli_bullets(c(
            " " = "Background process PID: {result$pid}",
            " " = "R version in background: {result$r_version}"
          ))
          TRUE
        } else {
          cli::cli_alert_danger("Background process returned unexpected result")
          FALSE
        }
      },
      error = function(e) {
        cli::cli_alert_danger("Background process failed: {e$message}")
        cli::cli_bullets(c(
          "i" = "This may be caused by:",
          " " = "- Rtools not installed (required on Windows)",
          " " = "- Antivirus blocking process spawning",
          " " = "- Firewall or security policies",
          " " = "- Insufficient permissions"
        ))
        
        # Additional Windows-specific checks
        if (.Platform$OS.type == "windows") {
          cli::cli_text("")
          cli::cli_alert_info("Windows-specific suggestions:")
          cli::cli_bullets(c(
            "*" = "Install Rtools from: https://cran.r-project.org/bin/windows/Rtools/",
            "*" = "Ensure R is in your PATH",
            "*" = "Try running R as Administrator",
            "*" = "Check if antivirus is blocking Rscript.exe"
          ))
        }
        FALSE
      }
    )
  }
  
  # Summary
  cli::cli_h2("Summary")
  
  all_ok <- ps_ok && pids_ok && callr_works
  
  if (all_ok) {
    cli::cli_alert_success("All checks passed! Background polling should work.")
  } else if (ps_ok && !callr_works) {
    cli::cli_alert_warning(
      "Memory queries work but background polling is unavailable."
    )
    cli::cli_bullets(c(
      "i" = "memtoc will work in snapshot-only mode (interval = NULL)",
      "i" = "To enable background polling, fix the callr issues above"
    ))
  } else {
    cli::cli_alert_danger("Some checks failed. See details above.")
  }
  
  invisible(list(
    ps_ok = ps_ok,
    pids_ok = pids_ok,
    sys_mem_ok = sys_mem_ok,
    callr_loaded = callr_loaded,
    callr_works = callr_works,
    all_ok = all_ok
  ))
}
