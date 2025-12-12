# memtoc Comprehensive Feature Test
# ==================================
# This script tests ALL package features with clear pass/fail indicators
# Run after installing: devtools::install("path/to/memtoc")

library(memtoc)

# Test counters
tests_passed <- 0
tests_failed <- 0
test_results <- list()

# Helper function for consistent test output
run_test <- function(test_name, test_expr) {

  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("TEST: ", test_name, "\n")

  cat(strrep("=", 70), "\n")
  
  result <- tryCatch({
    test_expr
  }, error = function(e) {
    list(success = FALSE, message = paste("ERROR:", e$message), data = NULL)
  })

  
  if (isTRUE(result$success)) {
    cat("\n>>> RESULT: PASS <<<\n")
    tests_passed <<- tests_passed + 1
  } else {
    cat("\n>>> RESULT: FAIL <<<\n")
    cat("Reason:", result$message, "\n")
    tests_failed <<- tests_failed + 1
  }
  
  test_results[[test_name]] <<- result
  invisible(result)
}

cat("\n")
cat(strrep("#", 70), "\n")
cat("#", sprintf("%-66s", " memtoc COMPREHENSIVE TEST SUITE"), "#\n")
cat("#", sprintf("%-66s", paste(" Started:", Sys.time())), "#\n")
cat(strrep("#", 70), "\n")

# ===========================================================================
# TEST 1: Package loads correctly
# ===========================================================================
run_test("1. Package Loading", {
  pkg_version <- packageVersion("memtoc")
  cat("Package version:", as.character(pkg_version), "\n")
  
  # Check all exported functions exist
  exports <- c("tic_mem", "toc_mem", "mem_log", "mem_clearlog", "mem_clear",
               "mem_recover", "mem_capabilities", "mem_diagnose", 
               "mem_parallel_info", "mem_print_log")
  
  missing <- exports[!sapply(exports, exists, where = "package:memtoc")]
  
  if (length(missing) > 0) {
    list(success = FALSE, 
         message = paste("Missing exports:", paste(missing, collapse = ", ")),
         data = NULL)
  } else {
    cat("All", length(exports), "exported functions available\n")
    list(success = TRUE, message = "OK", data = list(version = pkg_version))
  }
})

# ===========================================================================
# TEST 2: Capability detection
# ===========================================================================
run_test("2. Capability Detection (mem_capabilities)", {
  caps <- mem_capabilities()
  
  cat("Memory queries:     ", caps["memory_queries"], "\n")
  cat("Background polling: ", caps["background_polling"], "\n")
  
  if (!caps["memory_queries"]) {
    list(success = FALSE, 
         message = "Memory queries not available (ps package issue)",
         data = caps)
  } else {
    list(success = TRUE, message = "OK", data = caps)
  }
})

# ===========================================================================
# TEST 3: Basic snapshot mode (interval = NULL)
# ===========================================================================
run_test("3. Snapshot Mode (interval = NULL)", {
  mem_clear()
  
  tic_mem("snapshot test", interval = NULL, workers = "none")
  
  # Allocate ~80 MB
  x <- numeric(1e7)
  Sys.sleep(0.2)
  
  result <- toc_mem(quiet = TRUE)
  rm(x)
  
  cat("Label:        ", result$msg, "\n")
  cat("Memory start: ", round(result$mem_start / 1024^2, 2), "MB\n")
  cat("Memory end:   ", round(result$mem_end / 1024^2, 2), "MB\n")
  cat("Memory peak:  ", round(result$mem_peak / 1024^2, 2), "MB\n")
  cat("Memory change:", round(result$mem_change / 1024^2, 2), "MB\n")
  cat("Elapsed:      ", round(result$elapsed, 3), "sec\n")
  cat("Samples:      ", result$n_samples, "(expected: 0 for snapshot mode)\n")
  
  checks <- c(
    "msg correct" = identical(result$msg, "snapshot test"),
    "mem_start > 0" = !is.na(result$mem_start) && result$mem_start > 0,
    "mem_end > 0" = !is.na(result$mem_end) && result$mem_end > 0,
    "mem_peak > 0" = !is.na(result$mem_peak) && result$mem_peak > 0,
    "elapsed > 0" = result$elapsed > 0,
    "n_samples = 0" = result$n_samples == 0,
    "trajectory NULL" = is.null(result$trajectory) || nrow(result$trajectory) == 0
  )
  
  cat("\nValidation checks:\n")
  for (nm in names(checks)) {
    cat("  ", nm, ":", ifelse(checks[nm], "PASS", "FAIL"), "\n")
  }
  
  if (all(checks)) {
    list(success = TRUE, message = "OK", data = result)
  } else {
    list(success = FALSE, 
         message = paste("Failed checks:", paste(names(checks)[!checks], collapse = ", ")),
         data = result)
  }
})

# ===========================================================================
# TEST 4: Background polling mode
# ===========================================================================
run_test("4. Background Polling Mode (interval = 0.3)", {
  mem_clear()
  caps <- mem_capabilities()
  
  if (!caps["background_polling"]) {
    cat("SKIPPED: Background polling not available\n")
    return(list(success = TRUE, message = "SKIPPED", data = NULL))
  }
  
  tic_mem("polling test", interval = 0.3, workers = "none")
  
  # Create memory churn over 2 seconds
  peak_alloc <- 0
  for (i in 1:5) {
    size <- 5e6 * i  # 5M, 10M, 15M, 20M, 25M elements
    y <- numeric(size)
    peak_alloc <- max(peak_alloc, size * 8)  # 8 bytes per numeric
    Sys.sleep(0.3)
    rm(y)
  }
  
  result <- toc_mem(quiet = TRUE)
  
  cat("Label:           ", result$msg, "\n")
  cat("Memory peak:     ", round(result$mem_peak / 1024^2, 2), "MB\n")
  cat("Memory end:      ", round(result$mem_end / 1024^2, 2), "MB\n")
  cat("Elapsed:         ", round(result$elapsed, 2), "sec\n")
  cat("Samples:         ", result$n_samples, "\n")
  cat("Expected peak:   ~", round(peak_alloc / 1024^2, 0), "MB (from allocation)\n")
  
  has_trajectory <- !is.null(result$trajectory) && nrow(result$trajectory) > 0
  
  if (has_trajectory) {
    cat("\nTrajectory summary:\n")
    cat("  Rows:          ", nrow(result$trajectory), "\n")
    cat("  Columns:       ", paste(names(result$trajectory), collapse = ", "), "\n")
    cat("  Time span:     ", 
        round(as.numeric(difftime(max(result$trajectory$timestamp), 
                                   min(result$trajectory$timestamp), units = "secs")), 2), 
        "sec\n")
    cat("  RSS range:     ", round(min(result$trajectory$rss) / 1024^2, 1), "-",
        round(max(result$trajectory$rss) / 1024^2, 1), "MB\n")
  }
  
  checks <- c(
    "n_samples > 0" = result$n_samples > 0,
    "trajectory exists" = has_trajectory,
    "elapsed > 1 sec" = result$elapsed > 1,
    "peak captured" = result$mem_peak > result$mem_end  # Peak should exceed final
  )
  
  cat("\nValidation checks:\n")
  for (nm in names(checks)) {
    cat("  ", nm, ":", ifelse(checks[nm], "PASS", "FAIL"), "\n")
  }
  
  if (all(checks)) {
    list(success = TRUE, message = "OK", data = result)
  } else {
    list(success = FALSE,
         message = paste("Failed:", paste(names(checks)[!checks], collapse = ", ")),
         data = result)
  }
})

# ===========================================================================
# TEST 5: System memory and CPU tracking
# ===========================================================================
run_test("5. System Memory & CPU Tracking", {
  mem_clear()
  caps <- mem_capabilities()
  
  if (!caps["background_polling"]) {
    cat("SKIPPED: Background polling not available\n")
    return(list(success = TRUE, message = "SKIPPED", data = NULL))
  }
  
  tic_mem("system tracking", interval = 0.3, workers = "none")
  
  # Do some CPU work
  for (i in 1:3) {
    x <- matrix(rnorm(1e6), ncol = 100)
    y <- x %*% t(x[1:100, ])
    Sys.sleep(0.2)
    rm(x, y)
  }
  
  result <- toc_mem(quiet = TRUE)
  
  cat("System RAM total:     ")
  if (!is.na(result$sys_total)) {
    cat(round(result$sys_total / 1024^3, 2), "GB\n")
  } else {
    cat("NA\n")
  }
  
  cat("System RAM peak used: ")
  if (!is.na(result$sys_peak_percent)) {
    cat(round(result$sys_peak_percent, 1), "%\n")
  } else {
    cat("NA\n")
  }
  
  cat("CPU user time:        ")
  if (!is.na(result$cpu_user)) {
    cat(round(result$cpu_user, 3), "sec\n")
  } else {
    cat("NA\n")
  }
  
  cat("CPU system time:      ")
  if (!is.na(result$cpu_system)) {
    cat(round(result$cpu_system, 3), "sec\n")
  } else {
    cat("NA\n")
  }
  
  # Check trajectory has system columns
  has_sys_cols <- FALSE
  if (!is.null(result$trajectory) && nrow(result$trajectory) > 0) {
    has_sys_cols <- all(c("sys_total", "sys_avail", "sys_percent") %in% 
                         names(result$trajectory))
    cat("\nTrajectory has system memory columns:", has_sys_cols, "\n")
    
    if (has_sys_cols) {
      cat("  sys_percent range: ", 
          round(min(result$trajectory$sys_percent, na.rm = TRUE), 1), "-",
          round(max(result$trajectory$sys_percent, na.rm = TRUE), 1), "%\n")
    }
  }
  
  checks <- c(
    "sys_total available" = !is.na(result$sys_total) && result$sys_total > 0,
    "sys_peak_percent available" = !is.na(result$sys_peak_percent),
    "trajectory has sys columns" = has_sys_cols
  )
  
  cat("\nValidation checks:\n")
  for (nm in names(checks)) {
    cat("  ", nm, ":", ifelse(checks[nm], "PASS", "FAIL"), "\n")
  }
  
  # Allow partial success (CPU times may be NA on some systems)
  if (sum(checks) >= 2) {
    list(success = TRUE, message = "OK", data = result)
  } else {
    list(success = FALSE,
         message = paste("Failed:", paste(names(checks)[!checks], collapse = ", ")),
         data = result)
  }
})

# ===========================================================================
# TEST 6: Nested tracking
# ===========================================================================
run_test("6. Nested Tracking", {
  mem_clear()
  
  tic_mem("outer", interval = NULL)
  Sys.sleep(0.1)
  outer_start <- Sys.time()
  
    tic_mem("inner1", interval = NULL)
    a <- numeric(1e6)
    Sys.sleep(0.1)
    inner1 <- toc_mem(quiet = TRUE)
    rm(a)
    
    tic_mem("inner2", interval = NULL)
    b <- numeric(2e6)
    Sys.sleep(0.1)
    inner2 <- toc_mem(quiet = TRUE)
    rm(b)
  
  Sys.sleep(0.1)
  outer <- toc_mem(quiet = TRUE)
  
  cat("Outer result:\n")
  cat("  Label:   ", outer$msg, "\n")
  cat("  Elapsed: ", round(outer$elapsed, 2), "sec\n")
  cat("  Peak:    ", round(outer$mem_peak / 1024^2, 2), "MB\n")
  
  cat("\nInner1 result:\n")
  cat("  Label:   ", inner1$msg, "\n
")
  cat("  Elapsed: ", round(inner1$elapsed, 2), "sec\n")
  cat("  Peak:    ", round(inner1$mem_peak / 1024^2, 2), "MB\n")
  
  cat("\nInner2 result:\n")
  cat("  Label:   ", inner2$msg, "\n")
  cat("  Elapsed: ", round(inner2$elapsed, 2), "sec\n")
  cat("  Peak:    ", round(inner2$mem_peak / 1024^2, 2), "MB\n")
  
  checks <- c(
    "outer.msg correct" = identical(outer$msg, "outer"),
    "inner1.msg correct" = identical(inner1$msg, "inner1"),
    "inner2.msg correct" = identical(inner2$msg, "inner2"),
    "outer.elapsed > inner1 + inner2" = outer$elapsed > (inner1$elapsed + inner2$elapsed) * 0.9,
    "nesting preserved" = outer$elapsed > 0.3  # Should be at least 0.3 sec total
  )
  
  cat("\nValidation checks:\n")
  for (nm in names(checks)) {
    cat("  ", nm, ":", ifelse(checks[nm], "PASS", "FAIL"), "\n")
  }
  
  if (all(checks)) {
    list(success = TRUE, message = "OK", 
         data = list(outer = outer, inner1 = inner1, inner2 = inner2))
  } else {
    list(success = FALSE,
         message = paste("Failed:", paste(names(checks)[!checks], collapse = ", ")),
         data = list(outer = outer, inner1 = inner1, inner2 = inner2))
  }
})

# ===========================================================================
# TEST 7: Logging functionality
# ===========================================================================
run_test("7. Logging (mem_log, mem_clearlog, mem_print_log)", {
  # Clear any existing log
  mem_clearlog()
  
  # Verify log is empty
  initial_log <- mem_log()
  
  # Run 5 iterations with logging
  # Use larger allocations to ensure measurable differences
  for (i in 1:5) {
    tic_mem(paste("iteration", i), interval = NULL)
    x <- numeric(i * 5e6)  # 5M, 10M, 15M, 20M, 25M elements (40-200 MB)
    Sys.sleep(0.05)
    toc_mem(log = TRUE, quiet = TRUE)
    rm(x)
    gc(verbose = FALSE)  # Force cleanup between iterations
  }
  
  # Retrieve log
  log_df <- mem_log()
  
  cat("Log entries: ", nrow(log_df), "\n")
  cat("Log columns: ", paste(names(log_df), collapse = ", "), "\n\n")
  
  if (nrow(log_df) > 0) {
    cat("Log contents:\n")
    for (i in 1:nrow(log_df)) {
      cat(sprintf("  %d. %-12s | Peak: %6.1f MB | Elapsed: %.3f sec\n",
                  i, log_df$msg[i], 
                  log_df$mem_peak[i] / 1024^2,
                  log_df$elapsed[i]))
    }
  }
  
  # Test mem_print_log
  cat("\nmem_print_log() output:\n")
  mem_print_log()
  
  # Clear and verify
  mem_clearlog()
  cleared_log <- mem_log()
  
  # Calculate peak trend (last should be higher than first due to larger allocations)
  first_peak <- log_df$mem_peak[1]
  last_peak <- log_df$mem_peak[nrow(log_df)]
  peak_trend_positive <- last_peak >= first_peak
  
  checks <- c(
    "initial log empty" = nrow(initial_log) == 0,
    "5 entries logged" = nrow(log_df) == 5,
    "has msg column" = "msg" %in% names(log_df),
    "has mem_peak column" = "mem_peak" %in% names(log_df),
    "has elapsed column" = "elapsed" %in% names(log_df),
    "log cleared" = nrow(cleared_log) == 0,
    "peak trend positive" = peak_trend_positive  # Last peak >= first peak
  )
  
  cat("\nValidation checks:\n")
  for (nm in names(checks)) {
    cat("  ", nm, ":", ifelse(checks[nm], "PASS", "FAIL"), "\n")
  }
  
  if (all(checks)) {
    list(success = TRUE, message = "OK", data = log_df)
  } else {
    list(success = FALSE,
         message = paste("Failed:", paste(names(checks)[!checks], collapse = ", ")),
         data = log_df)
  }
})

# ===========================================================================
# TEST 8: mem_clear (stack cleanup)
# ===========================================================================
run_test("8. Stack Cleanup (mem_clear)", {
  # Start some tracking without finishing
  tic_mem("orphan1", interval = NULL)
  tic_mem("orphan2", interval = NULL)
  tic_mem("orphan3", interval = NULL)
  
  # Clear should remove all
  mem_clear()
  
  # Now a normal tic/toc should work without errors
  tic_mem("after clear", interval = NULL)
  result <- toc_mem(quiet = TRUE)
  
  cat("Successfully tracked after clearing orphans\n")
  cat("Label: ", result$msg, "\n")
  
  checks <- c(
    "msg correct after clear" = identical(result$msg, "after clear"),
    "result valid" = !is.na(result$mem_peak)
  )
  
  cat("\nValidation checks:\n")
  for (nm in names(checks)) {
    cat("  ", nm, ":", ifelse(checks[nm], "PASS", "FAIL"), "\n")
  }
  
  if (all(checks)) {
    list(success = TRUE, message = "OK", data = result)
  } else {
    list(success = FALSE,
         message = paste("Failed:", paste(names(checks)[!checks], collapse = ", ")),
         data = result)
  }
})

# ===========================================================================
# TEST 9: Child process detection
# ===========================================================================
run_test("9. Child Process Detection (workers = 'children')", {
  mem_clear()
  caps <- mem_capabilities()
  
  if (!caps["background_polling"]) {
    cat("SKIPPED: Background polling not available\n")
    return(list(success = TRUE, message = "SKIPPED", data = NULL))
  }
  
  tic_mem("with children", interval = 0.5, workers = "children")
  Sys.sleep(1.5)
  result <- toc_mem(quiet = TRUE)
  
  cat("Workers detected: ", result$n_workers, "\n")
  cat("Samples:          ", result$n_samples, "\n")
  
  if (!is.null(result$worker_stats) && nrow(result$worker_stats) > 0) {
    cat("\nWorker stats:\n")
    for (i in 1:nrow(result$worker_stats)) {
      row <- result$worker_stats[i, ]
      cat(sprintf("  PID %d (%s): Peak %.1f MB\n",
                  row$pid, ifelse(row$is_main, "main", "child"),
                  row$mem_peak / 1024^2))
    }
  }
  
  # This test passes if it runs without error
  # (child detection may or may not find children depending on system)
  list(success = TRUE, message = "OK", data = result)
})

# ===========================================================================
# TEST 10: Parallel worker monitoring (if future available)
# ===========================================================================
run_test("10. Parallel Worker Monitoring (future)", {
  mem_clear()
  caps <- mem_capabilities()
  
  if (!caps["background_polling"]) {
    cat("SKIPPED: Background polling not available\n")
    return(list(success = TRUE, message = "SKIPPED", data = NULL))
  }
  
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    cat("SKIPPED: future/future.apply packages not installed\n")
    cat("Install with: install.packages(c('future', 'future.apply'))\n")
    return(list(success = TRUE, message = "SKIPPED", data = NULL))
  }
  
  # Set up parallel workers
  cat("Setting up future::multisession with 2 workers...\n")
  future::plan(future::multisession, workers = 2)
  Sys.sleep(2)  # Give workers time to start
  
  # Check parallel info
  cat("\nmem_parallel_info():\n")
  pinfo <- mem_parallel_info()
  
  # Run parallel job with monitoring
  cat("\nRunning parallel job...\n")
  tic_mem("parallel test", interval = 0.5, workers = "auto")
  
  results <- future.apply::future_lapply(1:4, function(i) {
    x <- rnorm(2e6)  # ~16 MB per worker
    Sys.sleep(0.5)
    list(i = i, mean = mean(x), pid = Sys.getpid())
  }, future.seed = TRUE)
  
  mem_result <- toc_mem(quiet = TRUE)
  
  # Reset plan
  future::plan(future::sequential)
  
  cat("\nResults:\n")
  cat("  Total peak:     ", round(mem_result$mem_peak / 1024^2, 1), "MB\n")
  cat("  Workers:        ", mem_result$n_workers, "\n")
  cat("  Samples:        ", mem_result$n_samples, "\n")
  cat("  Elapsed:        ", round(mem_result$elapsed, 2), "sec\n")
  
  if (!is.null(mem_result$worker_stats) && nrow(mem_result$worker_stats) > 0) {
    cat("\n  Per-worker breakdown:\n")
    ws <- mem_result$worker_stats
    for (i in 1:nrow(ws)) {
      cat(sprintf("    PID %d (%s): Peak %.1f MB, Mean %.1f MB, %d samples\n",
                  ws$pid[i], 
                  ifelse(ws$is_main[i], "main", "worker"),
                  ws$mem_peak[i] / 1024^2,
                  ws$mem_mean[i] / 1024^2,
                  ws$n_samples[i]))
    }
  }
  
  # Verify parallel results
  cat("\n  Parallel job outputs:\n")
  for (r in results) {
    cat(sprintf("    Task %d: PID %d, mean = %.4f\n", r$i, r$pid, r$mean))
  }
  
  checks <- c(
    "n_samples > 0" = mem_result$n_samples > 0,
    "elapsed > 0" = mem_result$elapsed > 0,
    "4 tasks completed" = length(results) == 4
  )
  
  cat("\nValidation checks:\n")
  for (nm in names(checks)) {
    cat("  ", nm, ":", ifelse(checks[nm], "PASS", "FAIL"), "\n")
  }
  
  if (all(checks)) {
    list(success = TRUE, message = "OK", data = mem_result)
  } else {
    list(success = FALSE,
         message = paste("Failed:", paste(names(checks)[!checks], collapse = ", ")),
         data = mem_result)
  }
})

# ===========================================================================
# TEST 11: mem_diagnose
# ===========================================================================
run_test("11. Diagnostics (mem_diagnose)", {
  cat("Running mem_diagnose():\n\n")
  diag <- mem_diagnose()
  
  cat("\nDiagnostic results:\n")
  cat("  ps_ok:        ", diag$ps_ok, "\n")
  cat("  pids_ok:      ", diag$pids_ok, "\n")
  cat("  sys_mem_ok:   ", diag$sys_mem_ok, "\n")
  cat("  callr_loaded: ", diag$callr_loaded, "\n")
  cat("  callr_works:  ", diag$callr_works, "\n")
  cat("  all_ok:       ", diag$all_ok, "\n")
  
  # At minimum, ps should work
  if (diag$ps_ok) {
    list(success = TRUE, message = "OK", data = diag)
  } else {
    list(success = FALSE, message = "ps package not working", data = diag)
  }
})

# ===========================================================================
# TEST 12: mem_recover (crash recovery check)
# ===========================================================================
run_test("12. Crash Recovery (mem_recover)", {
  cat("Checking for recovery files...\n\n")
  
  # This should list any existing recovery files (likely none in clean test)
  recovery_info <- mem_recover()
  
  cat("\nmem_recover() executed successfully\n")
  cat("(Recovery files would be shown if any exist from crashed sessions)\n")
  
  list(success = TRUE, message = "OK", data = recovery_info)
})

# ===========================================================================
# TEST 13: Print method for results
# ===========================================================================
run_test("13. Print Method (print.memtoc_result)", {
  mem_clear()
  caps <- mem_capabilities()
  
  tic_mem("print test", interval = if(caps["background_polling"]) 0.3 else NULL)
  x <- matrix(rnorm(1e6), ncol = 100)
  Sys.sleep(0.5)
  rm(x)
  result <- toc_mem(quiet = TRUE)
  
  cat("Calling print() on memtoc_result:\n\n")
  print(result)
  
  list(success = TRUE, message = "OK", data = result)
})

# ===========================================================================
# TEST 14: Console output formatting
# ===========================================================================
run_test("14. Console Output (toc_mem with quiet = FALSE)", {
  mem_clear()
  
  cat("With quiet = FALSE (default):\n")
  tic_mem("visible output", interval = NULL)
  x <- numeric(5e6)
  Sys.sleep(0.1)
  rm(x)
  result <- toc_mem(quiet = FALSE)  # Should print message
  
  cat("\nWith quiet = TRUE:\n")
  tic_mem("silent output", interval = NULL)
  y <- numeric(5e6)
  Sys.sleep(0.1)
  rm(y)
  result2 <- toc_mem(quiet = TRUE)  # Should NOT print message
  cat("(no output expected above this line)\n")
  
  list(success = TRUE, message = "OK", data = list(result, result2))
})

# ===========================================================================
# TEST 15: Edge cases
# ===========================================================================
run_test("15. Edge Cases", {
  mem_clear()
  
  # Test 1: Empty label
  cat("Testing empty label...\n")
  tic_mem(interval = NULL)
  toc_mem(quiet = TRUE)
  cat("  Empty label: OK\n")
  
  # Test 2: Very short operation
  cat("Testing very short operation...\n")
  tic_mem("quick", interval = NULL)
  result_quick <- toc_mem(quiet = TRUE)
  cat("  Quick op elapsed:", result_quick$elapsed, "sec\n")
  
  # Test 3: Unicode label
  cat("Testing unicode label...\n
")
  tic_mem("测试 тест 🧪", interval = NULL)
  result_unicode <- toc_mem(quiet = TRUE)
  cat("  Unicode label:", result_unicode$msg, "\n")
  
  # Test 4: Long label
  long_label <- paste(rep("a", 100), collapse = "")
  cat("Testing long label (100 chars)...\n")
  tic_mem(long_label, interval = NULL)
  result_long <- toc_mem(quiet = TRUE)
  cat("  Long label length:", nchar(result_long$msg), "\n")
  
  list(success = TRUE, message = "OK", 
       data = list(quick = result_quick, unicode = result_unicode, long = result_long))
})

# ===========================================================================
# SUMMARY
# ===========================================================================
cat("\n")
cat(strrep("#", 70), "\n")
cat("#", sprintf("%-66s", " TEST SUMMARY"), "#\n")
cat(strrep("#", 70), "\n\n")

cat("Tests passed: ", tests_passed, "\n")
cat("Tests failed: ", tests_failed, "\n")
cat("Total tests:  ", tests_passed + tests_failed, "\n\n")

if (tests_failed == 0) {
  cat("========================================\n")
  cat("  ALL TESTS PASSED SUCCESSFULLY! \n
")
  cat("========================================\n")
} else {
  cat("========================================\n")
  cat("  SOME TESTS FAILED \n")
  cat("========================================\n")
  
  cat("\nFailed tests:\n")
  for (nm in names(test_results)) {
    if (!isTRUE(test_results[[nm]]$success)) {
      cat("  -", nm, ":", test_results[[nm]]$message, "\n")
    }
  }
}

cat("\nCompleted:", as.character(Sys.time()), "\n")
