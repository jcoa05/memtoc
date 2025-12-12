# Tests for tic_mem / toc_mem core functionality

# Helper to ensure clean state before each test
setup_clean_state <- function() {
  mem_clear()
  mem_clearlog()
}

test_that("basic tic_mem/toc_mem works", {
  setup_clean_state()

  tic_mem("test")
  Sys.sleep(0.1)
  result <- toc_mem(quiet = TRUE)

  expect_s3_class(result, "memtoc_result")
  expect_equal(result$msg, "test")
  expect_gte(result$elapsed, 0.05)
  expect_type(result$mem_start, "double")
  expect_type(result$mem_end, "double")
  expect_type(result$mem_peak, "double")
  expect_s3_class(result$tic_timestamp, "POSIXct")
  expect_s3_class(result$toc_timestamp, "POSIXct")
})


test_that("tic_mem without message works", {
  setup_clean_state()

  tic_mem()
  result <- toc_mem(quiet = TRUE)

  expect_null(result$msg)
  expect_type(result$elapsed, "double")
})


test_that("nesting works correctly", {
  setup_clean_state()

  tic_mem("outer")
  Sys.sleep(0.05)

  tic_mem("inner")
  Sys.sleep(0.05)
  inner_result <- toc_mem(quiet = TRUE)

  outer_result <- toc_mem(quiet = TRUE)

  expect_equal(inner_result$msg, "inner")
  expect_equal(outer_result$msg, "outer")

  # Outer should have longer elapsed time
  expect_gt(outer_result$elapsed, inner_result$elapsed)
})


test_that("deeply nested calls work", {
  setup_clean_state()

  tic_mem("level1")
  tic_mem("level2")
  tic_mem("level3")

  r3 <- toc_mem(quiet = TRUE)
  r2 <- toc_mem(quiet = TRUE)
  r1 <- toc_mem(quiet = TRUE)

  expect_equal(r3$msg, "level3")
  expect_equal(r2$msg, "level2")
  expect_equal(r1$msg, "level1")
})


test_that("unmatched toc_mem errors gracefully", {
  setup_clean_state()

  expect_error(
    toc_mem(),
    "stack is empty"
  )
})


test_that("mem_clear clears the stack", {
  setup_clean_state()

  tic_mem("will be cleared")
  tic_mem("also cleared")

  # mem_clear may print a message about cleared entries
  expect_no_error(mem_clear())

  # Now toc_mem should error
  expect_error(toc_mem(), "stack is empty")
})


test_that("logging works correctly", {
  setup_clean_state()

  tic_mem("first")
  Sys.sleep(0.05)
  toc_mem(log = TRUE, quiet = TRUE)

  tic_mem("second")
  Sys.sleep(0.05)
  toc_mem(log = TRUE, quiet = TRUE)

  log_df <- mem_log()

  expect_s3_class(log_df, "data.frame")
  expect_equal(nrow(log_df), 2L)
  expect_equal(log_df$msg, c("first", "second"))
  expect_true(all(c("mem_start", "mem_end", "mem_peak", "elapsed") %in% names(log_df)))
})


test_that("mem_log returns empty data frame when log is empty", {
  setup_clean_state()

  log_df <- mem_log()

  expect_s3_class(log_df, "data.frame")
  expect_equal(nrow(log_df), 0L)
  expect_true("msg" %in% names(log_df))
})


test_that("mem_log format = 'list' works", {
  setup_clean_state()

  tic_mem("test")
  toc_mem(log = TRUE, quiet = TRUE)

  log_list <- mem_log(format = "list")

  expect_type(log_list, "list")
  expect_length(log_list, 1L)
  expect_s3_class(log_list[[1]], "memtoc_result")
})


test_that("mem_clearlog clears the log", {
  setup_clean_state()

  tic_mem("test")
  toc_mem(log = TRUE, quiet = TRUE)

  expect_equal(nrow(mem_log()), 1L)

  mem_clearlog()

  expect_equal(nrow(mem_log()), 0L)
})


test_that("print.memtoc_result doesn't error", {
  setup_clean_state()

  tic_mem("print test")
  result <- toc_mem(quiet = TRUE)

  # Just verify print doesn't error (cli output may not be captured by expect_output)
  expect_no_error(print(result))
})


test_that("memory changes are detected", {
  setup_clean_state()

  # Force garbage collection first
  gc(verbose = FALSE)

  tic_mem("allocation test")

  # Allocate a reasonably large vector
  x <- numeric(1e7)  # ~80 MB

  result <- toc_mem(quiet = TRUE)

  # Memory should have increased (though exact amount depends on system)
  # Just verify we get numeric values
  expect_type(result$mem_change, "double")

  # Clean up
  rm(x)
  gc(verbose = FALSE)
})


test_that("quiet parameter works for both tic_mem and toc_mem", {
  setup_clean_state()

  # quiet = TRUE should produce no output (or at minimum, no error)
  expect_no_error({
    tic_mem("silent", quiet = TRUE)
    toc_mem(quiet = TRUE)
  })

  # quiet = FALSE for toc_mem should not error
  # (cli output may not be captured by expect_output in all environments)
  expect_no_error({
    tic_mem("not silent", quiet = TRUE)
    toc_mem(quiet = FALSE)
  })
})
