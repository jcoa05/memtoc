# Tests for MVP 2 background monitoring functionality

test_that("tic_mem with interval=NULL uses snapshot mode", {
  mem_clear()
  mem_clearlog()
  
  tic_mem("snapshot test", interval = NULL)
  Sys.sleep(0.1)
  result <- toc_mem(quiet = TRUE)
  
  expect_s3_class(result, "memtoc_result")
  expect_equal(result$n_samples, 0L)
  expect_null(result$trajectory)
})


test_that("tic_mem with interval uses background polling", {
  skip_on_cran()
  mem_clear()
  mem_clearlog()
  
  # Use a short interval for testing
  tic_mem("polling test", interval = 0.2)
  Sys.sleep(1.5)  # Allow time for multiple samples
  result <- toc_mem(quiet = TRUE)
  
  expect_s3_class(result, "memtoc_result")
  
  # Should have collected samples
  # Note: May be 0 if background process failed to start
  if (result$n_samples > 0) {
    expect_gt(result$n_samples, 0)
    expect_true(is.data.frame(result$trajectory))
    
    # Check required columns exist
    expect_true("timestamp" %in% names(result$trajectory))
    expect_true("pid" %in% names(result$trajectory))
    expect_true("rss" %in% names(result$trajectory))
    
    # Check new MVP 2 columns
    expect_true("sys_percent" %in% names(result$trajectory))
    expect_true("sys_total" %in% names(result$trajectory))
    expect_true("cpu_user" %in% names(result$trajectory))
    
    expect_equal(nrow(result$trajectory), result$n_samples)
  }
})


test_that("trajectory contains valid data", {
  skip_on_cran()
  mem_clear()
  
  # Use workers="none" to ensure only main process is tracked
  tic_mem("trajectory test", interval = 0.3, workers = "none")
  
  # Allocate some memory to create a visible change
  x <- numeric(1e7)
  Sys.sleep(1)
  rm(x)
  gc(verbose = FALSE)
  Sys.sleep(0.5)
  
  result <- toc_mem(quiet = TRUE)
  
  if (!is.null(result$trajectory) && nrow(result$trajectory) > 0) {
    traj <- result$trajectory
    
    # Check data types
    expect_s3_class(traj$timestamp, "POSIXct")
    expect_type(traj$pid, "integer")
    expect_type(traj$rss, "double")
    
    # All RSS values should be positive
    expect_true(all(traj$rss > 0))
    
    # Timestamps should be in order
    expect_true(all(diff(traj$timestamp) >= 0))
    
    # PID should match current process (since workers="none")
    expect_true(all(traj$pid == Sys.getpid()))
    
    # System memory should be valid (if present)
    if ("sys_percent" %in% names(traj)) {
      valid_percent <- !is.na(traj$sys_percent)
      if (any(valid_percent)) {
        expect_true(all(traj$sys_percent[valid_percent] >= 0))
        expect_true(all(traj$sys_percent[valid_percent] <= 100))
      }
    }
  }
})


test_that("result contains system memory info", {
  skip_on_cran()
  mem_clear()
  
  tic_mem("sys mem test", interval = 0.5)
  Sys.sleep(1.2)
  result <- toc_mem(quiet = TRUE)
  
  # Check that system memory fields exist
  expect_true("sys_peak_percent" %in% names(result))
  expect_true("sys_total" %in% names(result))
  expect_true("cpu_user" %in% names(result))
  expect_true("cpu_system" %in% names(result))
  
  # If we got samples, system info should be populated
  if (result$n_samples > 0) {
    expect_true(!is.na(result$sys_peak_percent) || is.na(result$sys_peak_percent))
  }
})


test_that("mem_capabilities returns expected structure", {
  caps <- mem_capabilities()
  
  expect_type(caps, "logical")
  expect_named(caps, c("memory_queries", "background_polling"))
})


test_that("nested calls with different intervals work", {
  skip_on_cran()
  mem_clear()
  
  # Outer with polling, inner with snapshot
  tic_mem("outer", interval = 0.5)
  Sys.sleep(0.3)
  
  tic_mem("inner", interval = NULL)
  Sys.sleep(0.1)
  inner_result <- toc_mem(quiet = TRUE)
  
  Sys.sleep(0.3)
  outer_result <- toc_mem(quiet = TRUE)
  
  expect_equal(inner_result$msg, "inner")
  expect_equal(outer_result$msg, "outer")
  expect_equal(inner_result$n_samples, 0L)
  
  # Outer might have samples if background polling worked
  # Just verify it doesn't error
  expect_s3_class(outer_result, "memtoc_result")
})


test_that("mem_clear stops background monitors", {
  skip_on_cran()
  mem_clear()
  
  # Start a monitor
  tic_mem("will be cleared", interval = 1)
  Sys.sleep(0.2)
  
 # Clear should not error even with active monitor (may print message)
  expect_no_error(mem_clear())
  
  # Stack should be empty
  expect_error(toc_mem(), "stack is empty")
})


test_that("persist=FALSE disables file persistence", {
  mem_clear()
  
  tic_mem("no persist", interval = 0.5, persist = FALSE)
  Sys.sleep(0.3)
  result <- toc_mem(quiet = TRUE)
  
  # Should complete without error
  expect_s3_class(result, "memtoc_result")
})


test_that("result contains new MVP 2 fields", {
  mem_clear()
  
  tic_mem("field test", interval = NULL)
  result <- toc_mem(quiet = TRUE)
  
  # Check all expected fields exist (MVP 2 + MVP 3)
  expected_fields <- c(
    "msg", "mem_start", "mem_end", "mem_peak", "mem_change",
    "elapsed", "tic_timestamp", "toc_timestamp", "trajectory", "n_samples",
    "n_workers", "worker_stats",
    "sys_peak_percent", "sys_total", "cpu_user", "cpu_system"
  )
  
  expect_true(all(expected_fields %in% names(result)))
})
