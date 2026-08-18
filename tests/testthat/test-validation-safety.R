test_that("validate must be a function and must return exactly TRUE", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) writeLines("ok", destfile),
    .package = "stow"
  )

  expect_error(stow(url, "testPackage", validate = 1), "must be NULL or a function")
  expect_error(
    stow(url, "testPackage", validate = function(path) 1, quiet = TRUE),
    "exactly TRUE"
  )
  expect_error(
    stow(
      url,
      "testPackage",
      validate = function(path) stop("bad format"),
      quiet = TRUE
    ),
    "validator error: bad format"
  )
})

test_that("invalid online cache content is refreshed", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  destination <- stow_cache_file(url)
  writeLines("bad", destination)
  downloads <- 0L
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      downloads <<- downloads + 1L
      writeLines("good", destfile)
    },
    .package = "stow"
  )
  validator <- function(path) identical(readLines(path), "good")

  result <- stow(url, "testPackage", validate = validator, quiet = TRUE)
  expect_equal(downloads, 1L)
  expect_identical(result, normalizePath(destination, winslash = "/"))
  expect_identical(readLines(destination), "good")
})

test_that("invalid offline cache content errors without deletion", {
  local_stow_data_dir()
  url <- "https://example.com/files/data.csv"
  destination <- stow_cache_file(url)
  writeLines("bad", destination)

  expect_error(
    stow(
      url,
      "testPackage",
      offline = TRUE,
      quiet = TRUE,
      validate = function(path) FALSE
    ),
    "left unchanged"
  )
  expect_true(file.exists(destination))
  expect_identical(readLines(destination), "bad")
})

test_that("new downloads are validated before commit and temporaries are cleaned", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) writeLines("bad", destfile),
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", validate = function(path) FALSE, quiet = TRUE),
    "not committed"
  )
  expect_false(file.exists(destination))
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("a failed replacement restores the previous destination", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  destination <- stow_cache_file(url)
  writeLines("original", destination)
  rename_calls <- 0L
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) writeLines("replacement", destfile),
    .stow_file_rename = function(from, to) {
      rename_calls <<- rename_calls + 1L
      if (rename_calls == 2L) {
        return(FALSE)
      }
      base::file.rename(from, to)
    },
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", overwrite = TRUE, quiet = TRUE),
    "existing destination was restored"
  )
  expect_identical(readLines(destination), "original")
  expect_length(
    list.files(dirname(destination), pattern = "backup", all.files = TRUE),
    0L
  )
})

test_that("download failures are actionable and clean partial files", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("partial", destfile)
      stop("connection lost")
    },
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", quiet = TRUE),
    paste0("URL: ", url),
    fixed = TRUE
  )
  expect_false(file.exists(destination))
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("a concurrent destination wins a non-overwrite race", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  destination <- stow_cache_file(url)
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("ours", destfile)
      writeLines("winner", destination)
    },
    .package = "stow"
  )

  result <- stow(
    url,
    "testPackage",
    quiet = TRUE,
    validate = function(path) identical(readLines(path), "ours") ||
      identical(readLines(path), "winner")
  )
  expect_identical(result, normalizePath(destination, winslash = "/"))
  expect_identical(readLines(destination), "winner")
  expect_length(
    list.files(dirname(destination), pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("quiet suppresses information but not warnings or errors", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) writeLines("ok", destfile),
    .package = "stow"
  )

  expect_silent(stow(url, "quietPackage", quiet = TRUE))
  expect_warning(
    stow(
      url,
      "warningPackage",
      quiet = TRUE,
      validate = function(path) {
        warning("validator warning")
        TRUE
      }
    ),
    "validator warning"
  )

  testthat::local_mocked_bindings(
    .stow_download_file = function(...) stop("still visible"),
    .package = "stow"
  )
  expect_error(
    stow(url, "errorPackage", quiet = TRUE),
    "still visible"
  )
})
