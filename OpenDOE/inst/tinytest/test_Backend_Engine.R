library(tinytest)

test_bg_process_success <- function() {
  bgp <- OpenDOE:::bg_process$new(com = OpenDOE:::backend_communicator)
  got <- NULL
  bgp$start(
    fun = function(x) x * 2,
    args = list(x = 21),
    on_success = function(res) got <<- res,
    on_finally = function() got <<- got
  )
  expect_true(bgp$running)
  while (bgp$running) {
    Sys.sleep(0.05)
    bgp$tick()
  }
  expect_equal(got, 42)
  expect_true(is.null(bgp$process))
  expect_false(bgp$running)
}
test_bg_process_success()

test_bg_process_error <- function() {
  bgp <- OpenDOE:::bg_process$new(com = OpenDOE:::backend_communicator)
  called <- FALSE
  bgp$start(
    fun = function() stop("boom"),
    args = list(),
    on_success = function(res) called <<- TRUE,
    on_finally = function() NULL
  )
  while (bgp$process$is_alive()) Sys.sleep(0.05)
  expect_error(bgp$tick(), "boom")
  expect_false(called)
  expect_true(grepl("boom", bgp$error))
}
test_bg_process_error()

test_bg_process_on_finally_runs_after_error <- function() {
  bgp <- OpenDOE:::bg_process$new(com = OpenDOE:::backend_communicator)
  finally_called <- FALSE
  bgp$start(
    fun = function() stop("boom"),
    args = list(),
    on_success = function(res) NULL,
    on_finally = function() finally_called <<- TRUE
  )
  while (bgp$process$is_alive()) Sys.sleep(0.05)
  expect_error(bgp$tick())
  expect_true(finally_called)
  expect_false(bgp$running)
}
test_bg_process_on_finally_runs_after_error()

test_bg_process_cancel_kills_before_natural_completion <- function() {
  bgp <- OpenDOE:::bg_process$new(com = OpenDOE:::backend_communicator)
  called <- FALSE
  finally_called <- FALSE
  bgp$start(
    fun = function() { Sys.sleep(30); "should never get here" },
    args = list(),
    on_success = function(res) called <<- TRUE,
    on_finally = function() finally_called <<- TRUE
  )
  expect_true(bgp$running)
  proc <- bgp$process

  start_time <- Sys.time()
  bgp$cancel()
  elapsed <- as.numeric(Sys.time() - start_time, units = "secs")

  expect_true(elapsed < 5)
  expect_false(proc$is_alive())
  expect_false(called)
  expect_true(finally_called)
  expect_false(bgp$running)
  expect_null(bgp$process)
  expect_silent(bgp$tick())
}
test_bg_process_cancel_kills_before_natural_completion()

test_bg_process_cancel_is_noop_when_nothing_running <- function() {
  bgp <- OpenDOE:::bg_process$new(com = OpenDOE:::backend_communicator)
  expect_silent(bgp$cancel())
  expect_false(bgp$running)
}
test_bg_process_cancel_is_noop_when_nothing_running()

test_bg_process_sync_fallback <- function() {
  old <- getOption("OpenDOE.background")
  options(OpenDOE.background = FALSE)
  on.exit(options(OpenDOE.background = old))
  bgp <- OpenDOE:::bg_process$new(com = OpenDOE:::backend_communicator)
  got <- NULL
  bgp$start(
    fun = function(x) x + 1,
    args = list(x = 1),
    on_success = function(res) got <<- res,
    on_finally = function() NULL
  )
  expect_equal(got, 2)
  expect_null(bgp$process)
}
test_bg_process_sync_fallback()
