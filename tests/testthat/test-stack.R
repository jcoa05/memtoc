# Tests for internal stack implementation

test_that("stack_push and stack_pop work correctly", {
  # Ensure clean state
  memtoc:::stack_clear()
  
  expect_true(memtoc:::stack_is_empty())
  expect_equal(memtoc:::stack_depth(), 0L)
  
  # Push items
  memtoc:::stack_push(list(value = 1))
  expect_false(memtoc:::stack_is_empty())
  expect_equal(memtoc:::stack_depth(), 1L)
  
  memtoc:::stack_push(list(value = 2))
  expect_equal(memtoc:::stack_depth(), 2L)
  
  # Pop items (LIFO order)
  item <- memtoc:::stack_pop()
  expect_equal(item$value, 2)
  expect_equal(memtoc:::stack_depth(), 1L)
  
  item <- memtoc:::stack_pop()
  expect_equal(item$value, 1)
  expect_equal(memtoc:::stack_depth(), 0L)
  
  expect_true(memtoc:::stack_is_empty())
})


test_that("stack_clear removes all items", {
  memtoc:::stack_clear()
  
  memtoc:::stack_push(list(a = 1))
  memtoc:::stack_push(list(b = 2))
  memtoc:::stack_push(list(c = 3))
  
  expect_equal(memtoc:::stack_depth(), 3L)
  
  memtoc:::stack_clear()
  
  expect_equal(memtoc:::stack_depth(), 0L)
  expect_true(memtoc:::stack_is_empty())
})


test_that("log functions work correctly", {
  memtoc:::log_clear()
  
  expect_equal(memtoc:::log_length(), 0L)
  
  # Push some results
  memtoc:::log_push(list(msg = "first", elapsed = 1.0))
  memtoc:::log_push(list(msg = "second", elapsed = 2.0))
  
  expect_equal(memtoc:::log_length(), 2L)
  
  entries <- memtoc:::log_get()
  expect_length(entries, 2L)
  expect_equal(entries[[1]]$msg, "first")
  expect_equal(entries[[2]]$msg, "second")
  
  # Clear log
  memtoc:::log_clear()
  expect_equal(memtoc:::log_length(), 0L)
})
