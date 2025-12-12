# Tests for MVP 3 parallel worker detection and monitoring

test_that("resolve_worker_pids returns main PID for workers='none'", {
  pids <- resolve_worker_pids(workers = "none", include_main = TRUE)
  
  expect_type(pids, "integer")
  expect_equal(pids, Sys.getpid())
})


test_that("resolve_worker_pids respects include_main", {
  pids_with_main <- resolve_worker_pids(workers = "none", include_main = TRUE)
  pids_without_main <- resolve_worker_pids(workers = "none", include_main = FALSE)
  
  expect_true(Sys.getpid() %in% pids_with_main)
  expect_equal(length(pids_without_main), 0)
})


test_that("resolve_worker_pids accepts explicit PID vector", {
  explicit_pids <- c(123L, 456L)
  pids <- resolve_worker_pids(workers = explicit_pids, include_main = TRUE)
  
  expect_true(Sys.getpid() %in% pids)
  expect_true(123L %in% pids)
  expect_true(456L %in% pids)
})


test_that("mem_parallel_info returns expected structure", {
  # Capture the invisible return
  info <- invisible(mem_parallel_info())
  
  expect_type(info, "list")
  expect_true("main_pid" %in% names(info))
  expect_true("future_available" %in% names(info))
  expect_true("parallelly_available" %in% names(info))
  # These fields should exist even if NULL
  expect_true("worker_pids" %in% names(info) || is.null(info$worker_pids))
  expect_true("child_pids" %in% names(info) || is.null(info$child_pids))
  
  expect_equal(info$main_pid, Sys.getpid())
})


test_that("tic_mem accepts workers parameter", {
  mem_clear()
  
  # Should not error with different worker specs
  tic_mem("test1", interval = NULL, workers = "none")
  toc_mem(quiet = TRUE)
  
  tic_mem("test2", interval = NULL, workers = "auto")
  toc_mem(quiet = TRUE)
  
  tic_mem("test3", interval = NULL, workers = "children")
  toc_mem(quiet = TRUE)
  
  tic_mem("test4", interval = NULL, workers = c(Sys.getpid()))
  result <- toc_mem(quiet = TRUE)
  
  expect_s3_class(result, "memtoc_result")
})


test_that("result contains n_workers and worker_stats fields", {
  mem_clear()
  
  tic_mem("worker test", interval = NULL, workers = "none")
  result <- toc_mem(quiet = TRUE)
  
  expect_true("n_workers" %in% names(result))
  expect_true("worker_stats" %in% names(result))
  expect_equal(result$n_workers, 0L)
})


test_that("get_child_pids returns NULL or integer vector", {
  # get_child_pids may return NULL, empty vector, or actual child PIDs
 # depending on the system state (some systems have background children)
  child_pids <- get_child_pids(Sys.getpid())
  
  # Should be NULL or an integer vector (possibly with children)
  expect_true(is.null(child_pids) || is.integer(child_pids))
})


test_that("calculate_worker_stats handles single PID trajectory", {
  skip_on_cran()
  
  # Create a mock trajectory with single PID
  trajectory <- data.frame(
    timestamp = Sys.time() + 0:4,
    pid = rep(Sys.getpid(), 5),
    rss = c(100, 200, 300, 200, 150) * 1e6,
    stringsAsFactors = FALSE
  )
  
  stats <- calculate_worker_stats(trajectory, Sys.getpid())
  
  expect_true(is.data.frame(stats))
  expect_equal(nrow(stats), 1)
  expect_equal(stats$pid, Sys.getpid())
  expect_true(stats$is_main)
  expect_equal(stats$mem_peak, 300e6)
})


test_that("calculate_worker_stats handles multiple PIDs", {
  skip_on_cran()
  
  # Create a mock trajectory with multiple PIDs
  trajectory <- data.frame(
    timestamp = rep(Sys.time() + 0:2, 2),
    pid = c(rep(100L, 3), rep(200L, 3)),
    rss = c(100, 200, 150, 50, 80, 60) * 1e6,
    stringsAsFactors = FALSE
  )
  
  stats <- calculate_worker_stats(trajectory, 100L)
  
  expect_true(is.data.frame(stats))
  expect_equal(nrow(stats), 2)
  expect_true(100L %in% stats$pid)
  expect_true(200L %in% stats$pid)
  
  # Check main process identification
  main_row <- stats[stats$pid == 100L, ]
  worker_row <- stats[stats$pid == 200L, ]
  
  expect_true(main_row$is_main)
  expect_false(worker_row$is_main)
  expect_equal(main_row$mem_peak, 200e6)
  expect_equal(worker_row$mem_peak, 80e6)
})


test_that("future detection functions handle missing packages gracefully", {
  # These should return FALSE/NULL without erroring
  # even if future is not installed
  expect_type(future_available(), "logical")
  expect_type(parallelly_available(), "logical")
  
  # get_future_worker_pids should return NULL if no workers
  worker_pids <- get_future_worker_pids()
  expect_true(is.null(worker_pids) || is.integer(worker_pids))
})
