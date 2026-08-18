test_that("replacement refuses a destination directory", {
  local_stow_data_dir()
  directory <- stow_path("testPackage")
  temp <- tempfile("download-", tmpdir = directory)
  destination <- file.path(directory, "occupied.csv")
  writeLines("replacement", temp)
  dir.create(destination)

  expect_error(
    stow:::.stow_commit_replacement(
      temp,
      destination,
      "https://example.com/occupied.csv"
    ),
    "expected file path is occupied by a directory"
  )
  expect_true(file.exists(temp))
  expect_true(dir.exists(destination))
})

test_that("stow refuses an occupied destination before downloading", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  destination <- stow_cache_file(url)
  dir.create(destination)
  testthat::local_mocked_bindings(
    .stow_download_file = function(...) stop("download called"),
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", quiet = TRUE),
    "expected file path is occupied by a directory"
  )
  expect_true(dir.exists(destination))
})

test_that("a download that produces no regular file is rejected", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  testthat::local_mocked_bindings(
    .stow_download_file = function(...) invisible(NULL),
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", quiet = TRUE),
    "download did not create a regular file"
  )
  expect_false(file.exists(destination))
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("a failed hard link cannot create a new cache entry", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("downloaded", destfile)
    },
    .stow_file_link = function(from, to) FALSE,
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", quiet = TRUE),
    "Could not atomically commit the downloaded file"
  )
  expect_false(file.exists(destination))
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("a destination created after link failure is treated as the race winner", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("ours", destfile)
    },
    .stow_file_link = function(from, to) {
      writeLines("winner", to)
      FALSE
    },
    .package = "stow"
  )

  expect_error(
    stow(
      url,
      "testPackage",
      quiet = TRUE,
      validate = function(path) identical(readLines(path), "ours")
    ),
    "concurrently created cache file failed validation"
  )
  expect_identical(readLines(destination), "winner")
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("a concurrent directory is left unchanged after link failure", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("ours", destfile)
    },
    .stow_file_link = function(from, to) {
      dir.create(to)
      FALSE
    },
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", quiet = TRUE),
    "Another process occupied the expected path with a non-file"
  )
  expect_true(dir.exists(destination))
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("replacement stops when the existing destination cannot be preserved", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  writeLines("original", destination)
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("replacement", destfile)
    },
    .stow_file_rename = function(from, to) FALSE,
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", overwrite = TRUE, quiet = TRUE),
    "Could not preserve the existing destination before replacement"
  )
  expect_identical(readLines(destination), "original")
  expect_length(
    list.files(directory, pattern = "backup", all.files = TRUE),
    0L
  )
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("a failed restoration retains the recovery backup", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  directory <- stow_path("testPackage")
  destination <- stow_cache_file(url)
  writeLines("original", destination)
  rename_calls <- 0L
  backup <- NULL
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("replacement", destfile)
    },
    .stow_file_rename = function(from, to) {
      rename_calls <<- rename_calls + 1L
      if (rename_calls == 1L) {
        backup <<- to
        return(base::file.rename(from, to))
      }
      if (rename_calls == 2L) {
        writeLines("interloper", destination)
      }
      FALSE
    },
    .package = "stow"
  )

  expect_error(
    stow(url, "testPackage", overwrite = TRUE, quiet = TRUE),
    "existing file could not be restored"
  )
  expect_equal(rename_calls, 3L)
  expect_false(file.exists(destination))
  expect_true(file.exists(backup))
  expect_identical(readLines(backup), "original")
  expect_length(
    list.files(directory, pattern = "^\\.stow-download-", all.files = TRUE),
    0L
  )
})

test_that("successful replacement warns when its backup cannot be removed", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  destination <- stow_cache_file(url)
  writeLines("original", destination)
  backup <- NULL
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      writeLines("replacement", destfile)
    },
    .stow_file_rename = function(from, to) {
      if (identical(from, destination)) {
        backup <<- to
      }
      base::file.rename(from, to)
    },
    .stow_unlink = function(path) {
      if (!is.null(backup) && identical(path, backup)) {
        return(1L)
      }
      base::unlink(path, force = TRUE)
    },
    .package = "stow"
  )

  expect_warning(
    result <- stow(
      url,
      "testPackage",
      overwrite = TRUE,
      quiet = TRUE
    ),
    "replacement succeeded, but its backup could not be removed"
  )
  expect_identical(result, normalizePath(destination, winslash = "/"))
  expect_identical(readLines(destination), "replacement")
  expect_true(file.exists(backup))
  expect_identical(readLines(backup), "original")
})
