# Tests for memory query functions

test_that("get_memory_info returns expected structure", {
  info <- memtoc:::get_memory_info()
  
  expect_type(info, "list")
  expect_named(info, c("rss", "vms", "timestamp", "pid", "success"))
  
  # On most systems, this should succeed
  if (info$success) {
    expect_type(info$rss, "double")
    expect_type(info$vms, "double")
    expect_s3_class(info$timestamp, "POSIXct")
    expect_type(info$pid, "integer")
    
    # RSS and VMS should be positive
    expect_gt(info$rss, 0)
    expect_gt(info$vms, 0)
    
    # PID should match current process
    expect_equal(info$pid, Sys.getpid())
  }
})


test_that("get_memory_info handles invalid PID gracefully", {
  # Use an invalid PID (very high number unlikely to exist)
  info <- memtoc:::get_memory_info(pid = 999999999L)
  
  expect_type(info, "list")
  expect_false(info$success)
  expect_true(is.na(info$rss))
})


test_that("memory_available returns logical", {
  result <- memtoc:::memory_available()
  expect_type(result, "logical")
  expect_length(result, 1L)
})


test_that("format_bytes handles various inputs correctly", {
  # Basic cases
  expect_equal(memtoc:::format_bytes(0), "0 B")
  expect_equal(memtoc:::format_bytes(500), "500 B")
  expect_equal(memtoc:::format_bytes(1024), "1 KB")
  expect_equal(memtoc:::format_bytes(1536), "1.5 KB")
  expect_equal(memtoc:::format_bytes(1048576), "1 MB")
  expect_equal(memtoc:::format_bytes(1073741824), "1 GB")
  
 # Edge cases
  expect_equal(memtoc:::format_bytes(NA), "NA")
  expect_equal(memtoc:::format_bytes(NULL), "NA")
  
  # Negative values
  expect_match(memtoc:::format_bytes(-1024), "-1 KB")
})


test_that("format_duration handles various inputs", {
  expect_equal(memtoc:::format_duration(0.5), "0.5 sec")
  expect_equal(memtoc:::format_duration(30), "30 sec")
  expect_match(memtoc:::format_duration(90), "1 min")
  expect_match(memtoc:::format_duration(3700), "1 hr")
  expect_equal(memtoc:::format_duration(NA), "NA")
})
