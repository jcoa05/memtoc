# memtoc MVP 3 Installation and Testing Script
# Run this in R to install and test the package

# Step 1: Install dependencies
install.packages(c("ps", "cli", "callr", "testthat"), repos = "https://cloud.r-project.org")

# Optional: Install future for parallel testing
# install.packages(c("future", "parallelly", "future.apply"), repos = "https://cloud.r-project.org")

# Step 2: Install memtoc from local source
# Adjust the path to where you extracted the package
# install.packages("path/to/memtoc", repos = NULL, type = "source")

# Or using devtools/pak:
# devtools::install_local("path/to/memtoc")
# pak::local_install("path/to/memtoc")

# Step 3: Quick tests
library(memtoc)

cat("\n========================================\n")
cat("memtoc MVP 3 Test Suite\n")
cat("========================================\n")

# Check capabilities
cat("\n=== Capability Check ===\n")
caps <- mem_capabilities()
print(caps)

if (!caps["memory_queries"]) {
  stop("Memory queries not available. Check ps package installation.")
}

# Check parallel info
cat("\n=== Parallel Backend Info ===\n")
mem_parallel_info()

# Test 1: Snapshot mode (interval = NULL)
cat("\n=== Test 1: Snapshot Mode ===\n")
tic_mem("snapshot test", interval = NULL, workers = "none")
x <- numeric(1e6)
Sys.sleep(0.3)
result1 <- toc_mem()
cat("Samples collected:", result1$n_samples, "(expected: 0)\n")
cat("Workers monitored:", result1$n_workers, "(expected: 0)\n")

# Test 2: Background polling with system memory
cat("\n=== Test 2: Background Polling + System Memory ===\n")
if (caps["background_polling"]) {
  tic_mem("polling test", interval = 0.3, workers = "none")
  
  # Create memory churn
  for (i in 1:5) {
    y <- matrix(rnorm(1e6), ncol = 100)
    Sys.sleep(0.2)
    rm(y)
  }
  
  result2 <- toc_mem()
  cat("Samples collected:", result2$n_samples, "\n")
  cat("System RAM peak usage:", round(result2$sys_peak_percent, 1), "%\n")
  cat("CPU user time:", round(result2$cpu_user, 3), "sec\n")
  
  if (!is.null(result2$trajectory) && nrow(result2$trajectory) > 0) {
    cat("\nTrajectory columns:\n")
    print(names(result2$trajectory))
  }
} else {
  cat("Skipped (background polling not available)\n")
}

# Test 3: Child process detection
cat("\n=== Test 3: Child Process Detection ===\n")
tic_mem("with children", interval = 0.5, workers = "children")
Sys.sleep(1)
result3 <- toc_mem()
cat("Workers detected:", result3$n_workers, "\n")

# Test 4: Parallel worker monitoring (if future is available)
cat("\n=== Test 4: Parallel Worker Monitoring ===\n")
if (requireNamespace("future", quietly = TRUE) && 
    requireNamespace("future.apply", quietly = TRUE) &&
    caps["background_polling"]) {
  
  cat("Setting up future::multisession with 2 workers...\n")
  future::plan(future::multisession, workers = 2)
  Sys.sleep(2)  # Give workers time to start
  
  # Check detection
  cat("\nParallel info after plan():\n")
  mem_parallel_info()
  
  # Monitor parallel job
  cat("\nRunning parallel job with monitoring...\n")
  tic_mem("parallel job", interval = 0.5, workers = "auto")
  
  result4 <- future.apply::future_lapply(1:4, function(i) {
    x <- rnorm(1e6)
    Sys.sleep(0.5)
    mean(x)
  }, future.seed = TRUE)
  
  parallel_result <- toc_mem()
  
  cat("\nResults:\n")
  cat("Total peak memory:", round(parallel_result$mem_peak / 1024^2, 1), "MB\n")
  cat("Workers detected:", parallel_result$n_workers, "\n")
  cat("Samples collected:", parallel_result$n_samples, "\n")
  
  if (!is.null(parallel_result$worker_stats)) {
    cat("\nPer-worker stats:\n")
    ws <- parallel_result$worker_stats
    ws$mem_peak_mb <- round(ws$mem_peak / 1024^2, 1)
    print(ws[, c("pid", "is_main", "mem_peak_mb", "n_samples")])
  }
  
  # Reset plan
  future::plan(future::sequential)
  
} else {
  cat("Skipped (future package not available or background polling disabled)\n")
  cat("Install future and future.apply to test parallel monitoring:\n")
  cat('  install.packages(c("future", "parallelly", "future.apply"))\n')
}

# Test 5: Nested calls
cat("\n=== Test 5: Nested Calls ===\n")
tic_mem("outer", interval = 0.5)
Sys.sleep(0.3)

  tic_mem("inner", interval = NULL)
  z <- rnorm(1e5)
  Sys.sleep(0.2)
  inner_result <- toc_mem()

Sys.sleep(0.3)
outer_result <- toc_mem()

cat("Inner samples:", inner_result$n_samples, "\n")
cat("Outer samples:", outer_result$n_samples, "\n")

# Test 6: Logging
cat("\n=== Test 6: Logging ===\n")
mem_clearlog()

for (i in 1:3) {
  tic_mem(paste("iteration", i), interval = NULL)
  w <- rnorm(1e5 * i)
  Sys.sleep(0.1)
  toc_mem(log = TRUE, quiet = TRUE)
  rm(w)
}

log_df <- mem_log()
cat("Log entries:", nrow(log_df), "\n")
print(log_df[, c("msg", "mem_peak", "elapsed")])

# Test 7: Stack Clear
cat("\n=== Test 7: Stack Clear ===\n")
tic_mem("will be cleared", interval = 0.5)
Sys.sleep(0.2)
mem_clear()

# Test 8: Full result print
cat("\n=== Test 8: Full Result Print ===\n")
if (caps["background_polling"]) {
  tic_mem("full result test", interval = 0.3)
  v <- matrix(rnorm(5e6), ncol = 500)
  Sys.sleep(1)
  rm(v)
  gc(verbose = FALSE)
  result_full <- toc_mem(quiet = TRUE)
  print(result_full)
}

# Test 9: Recovery check
cat("\n=== Test 9: Recovery Check ===\n")
mem_recover()

# Cleanup
rm(x, z)
gc(verbose = FALSE)

cat("\n========================================\n")
cat("All tests completed successfully!\n")
cat("========================================\n")
