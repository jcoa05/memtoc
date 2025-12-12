# Parallel Worker Detection Module
#
# This module provides functions for detecting and monitoring parallel
# workers from the future package ecosystem. It extracts PIDs from
# active future backends to enable memory tracking across workers.


#' Check if future package is available
#' @return Logical
#' @noRd
future_available <- function() {

  requireNamespace("future", quietly = TRUE)
}


#' Check if parallelly package is available
#' @return Logical
#' @noRd
parallelly_available <- function() {
  requireNamespace("parallelly", quietly = TRUE)
}


#' Get PIDs of active future workers
#'
#' Detects the current future plan and extracts worker process IDs.
#' Works with multisession, multicore, and cluster backends.
#'
#' @return Integer vector of worker PIDs, or NULL if no workers detected
#' @noRd
get_future_worker_pids <- function() {
  if (!future_available()) {
    return(NULL)
  }
  
tryCatch({
    # Get the current plan
    plan_info <- future::plan()
    plan_class <- class(plan_info)[1]
    
    # Check if it's a parallel plan
    if (plan_class %in% c("sequential", "uniprocess"))
 {
      return(NULL)
    }
    
    # Try to get workers from parallelly if available
    if (parallelly_available()) {
      pids <- get_pids_via_parallelly()
      if (!is.null(pids) && length(pids) > 0) {
        return(pids)
      }
    }
    
    # Fallback: try to get from future's internal structures
    pids <- get_pids_via_future_internals()
    if (!is.null(pids) && length(pids) > 0) {
      return(pids)
    }
    
    NULL
  }, error = function(e) {
    NULL
  })
}


#' Get worker PIDs using parallelly package
#' @return Integer vector of PIDs or NULL
#' @noRd
get_pids_via_parallelly <- function() {
  tryCatch({
    # Get the cluster workers
    workers <- parallelly::availableWorkers()
    
    # If it's a cluster, try to get the cluster object
    # and extract PIDs from node info
    plan_info <- future::plan()
    
    # Check for cluster attribute
    if (!is.null(attr(plan_info, "cluster"))) {
      cl <- attr(plan_info, "cluster")
      return(extract_cluster_pids(cl))
    }
    
    # Try to find cluster in future's globals
    cl <- get_active_cluster()
    if (!is.null(cl)) {
      return(extract_cluster_pids(cl))
    }
    
    NULL
  }, error = function(e) {
    NULL
  })
}


#' Get worker PIDs from future's internal structures
#' @return Integer vector of PIDs or NULL
#' @noRd
get_pids_via_future_internals <- function() {
  tryCatch({
    # Try to access future's internal worker tracking
    # This is backend-specific
    
    plan_info <- future::plan()
    plan_class <- class(plan_info)[1]
    
    if (plan_class == "multisession") {
      # multisession uses a PSOCK cluster internally
      # Try to find it in the plan's environment
      plan_env <- environment(plan_info)
      if (exists("workers", envir = plan_env)) {
        cl <- get("workers", envir = plan_env)
        return(extract_cluster_pids(cl))
      }
    }
    
    if (plan_class == "multicore") {
      # multicore uses forked processes
      # We can find children of current process
      return(get_child_pids(Sys.getpid()))
    }
    
    NULL
  }, error = function(e) {
    NULL
  })
}


#' Extract PIDs from a cluster object
#' @param cl A cluster object (from parallel or future)
#' @return Integer vector of PIDs
#' @noRd
extract_cluster_pids <- function(cl) {
  if (is.null(cl)) return(NULL)
  
  tryCatch({
    pids <- integer(0)
    
    # Iterate through cluster nodes
    for (i in seq_along(cl)) {
      node <- cl[[i]]
      
      # Try different ways to get PID
      pid <- NULL
      
      # Method 1: Direct pid attribute
      if (!is.null(node$pid)) {
        pid <- node$pid
      }
      
      # Method 2: con$pid (connection-based)
      if (is.null(pid) && !is.null(node$con) && !is.null(node$con$pid)) {
        pid <- node$con$pid
      }
      
      # Method 3: Query the worker directly
      if (is.null(pid)) {
        pid <- tryCatch({
          # Send Sys.getpid() to the worker
          parallel::clusterCall(cl[i], Sys.getpid)[[1]]
        }, error = function(e) NULL)
      }
      
      if (!is.null(pid) && is.numeric(pid)) {
        pids <- c(pids, as.integer(pid))
      }
    }
    
    if (length(pids) > 0) pids else NULL
  }, error = function(e) {
    NULL
  })
}


#' Get the active cluster from future's registry
#' @return Cluster object or NULL
#' @noRd
get_active_cluster <- function() {
  tryCatch({
    # Try to get from future's internal registry
    if (exists(".future", envir = globalenv())) {
      future_env <- get(".future", envir = globalenv())
      if (!is.null(future_env$cluster)) {
        return(future_env$cluster)
      }
    }
    
    # Try parallelly's method
    if (parallelly_available()) {
      cl <- tryCatch(
        parallelly::makeClusterPSOCK(workers = 0, autoStop = FALSE),
        error = function(e) NULL
      )
      return(cl)
    }
    
    NULL
  }, error = function(e) {
    NULL
  })
}


#' Get child process PIDs
#'
#' Uses ps::ps_children() to find all child processes of a given PID.
#'
#' @param pid Parent process ID
#' @param recursive Whether to include grandchildren
#' @return Integer vector of child PIDs
#' @noRd
get_child_pids <- function(pid, recursive = TRUE) {
  tryCatch({
    handle <- ps::ps_handle(pid)
    children <- ps::ps_children(handle, recursive = recursive)
    
    if (length(children) == 0) {
      return(NULL)
    }
    
    pids <- vapply(children, function(h) {
      tryCatch(ps::ps_pid(h), error = function(e) NA_integer_)
    }, integer(1))
    
    pids <- pids[!is.na(pids)]
    if (length(pids) > 0) pids else NULL
  }, error = function(e) {
    NULL
  })
}


#' Resolve worker specification to PIDs
#'
#' Takes a worker specification and returns a vector of PIDs to monitor.
#'
#' @param workers Worker specification: "auto", "none", or integer vector of PIDs
#' @param include_main Whether to include the main R process
#' @return Integer vector of PIDs to monitor
#' @noRd
resolve_worker_pids <- function(workers = "auto", include_main = TRUE) {
  main_pid <- Sys.getpid()
  
  if (is.character(workers)) {
    workers <- match.arg(workers, c("auto", "none", "children"))
    
    if (workers == "none") {
      return(if (include_main) main_pid else integer(0))
    }
    
    if (workers == "children") {
      child_pids <- get_child_pids(main_pid)
      pids <- if (include_main) c(main_pid, child_pids) else child_pids
      return(unique(as.integer(pids[!is.na(pids)])))
    }
    
    if (workers == "auto") {
      # Try to detect future workers
      worker_pids <- get_future_worker_pids()
      
      # Also get any child processes
      child_pids <- get_child_pids(main_pid)
      
      # Combine all PIDs
      all_pids <- c(
        if (include_main) main_pid else integer(0),
        worker_pids,
        child_pids
      )
      
      return(unique(as.integer(all_pids[!is.na(all_pids)])))
    }
  }
  
  if (is.numeric(workers)) {
    # Explicit PID list provided
    pids <- as.integer(workers)
    if (include_main && !(main_pid %in% pids)) {
      pids <- c(main_pid, pids)
    }
    return(unique(pids[!is.na(pids)]))
  }
  
  # Default: just main process
  if (include_main) main_pid else integer(0)
}


#' Get information about current parallel setup
#'
#' Returns diagnostic information about the detected parallel backend
#' and any workers that can be monitored.
#'
#' @return A list with parallel backend information
#' @export
#'
#' @examples
#' mem_parallel_info()
#'
#' # With future workers active
#' # future::plan(future::multisession, workers = 2)
#' # mem_parallel_info()
mem_parallel_info <- function() {
  info <- list(
    future_available = future_available(),
    parallelly_available = parallelly_available(),
    plan = NULL,
    n_workers = 0L,
    worker_pids = NULL,
    child_pids = NULL,
    main_pid = Sys.getpid()
  )
  
  if (info$future_available) {
    tryCatch({
      plan_info <- future::plan()
      info$plan <- class(plan_info)[1]
      
      # Try to get worker count
      if (parallelly_available()) {
        info$n_workers <- tryCatch(
          length(parallelly::availableWorkers()),
          error = function(e) 0L
        )
      }
    }, error = function(e) NULL)
  }
  
  # Get worker PIDs
  info$worker_pids <- get_future_worker_pids()
  
  # Get child PIDs
  info$child_pids <- get_child_pids(Sys.getpid())
  
  # Print summary
  cli::cli_h3("Parallel Backend Info")
  
  cli::cli_bullets(c(
    "*" = "Main process PID: {info$main_pid}",
    "*" = "future package: {if (info$future_available) 'available' else 'not installed'}",
    "*" = "parallelly package: {if (info$parallelly_available) 'available' else 'not installed'}"
  ))
  
  if (!is.null(info$plan)) {
    cli::cli_bullets(c(
      "*" = "Current plan: {info$plan}",
      "*" = "Workers configured: {info$n_workers}"
    ))
  }
  
  if (!is.null(info$worker_pids) && length(info$worker_pids) > 0) {
    cli::cli_bullets(c(
      "*" = "Detected worker PIDs: {paste(info$worker_pids, collapse = ', ')}"
    ))
  } else {
    cli::cli_bullets(c(
      "!" = "No active workers detected"
    ))
  }
  
  if (!is.null(info$child_pids) && length(info$child_pids) > 0) {
    cli::cli_bullets(c(
      "*" = "Child process PIDs: {paste(info$child_pids, collapse = ', ')}"
    ))
  }
  
  invisible(info)
}
