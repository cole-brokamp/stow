test_that("a valid managed local copy does not download", {
  local_stow_data_dir()
  local_no_etag()
  url <- "https://example.com/files/data.csv"
  destination <- stow_managed_copy_file(url)
  writeLines("stored", destination)
  testthat::local_mocked_bindings(
    .stow_download_file = function(...) stop("download called"),
    .package = "stow"
  )

  expect_message(
    result <- stow(url, "testPackage"),
    "Using managed local copy"
  )
  expect_identical(result, normalizePath(destination, winslash = "/"))
})

test_that("ETag changes create new variants and preserve old ones", {
  local_stow_data_dir()
  etag <- '"first"'
  downloads <- 0L
  testthat::local_mocked_bindings(
    .stow_probe_etag = function(url) etag,
    .stow_download_file = function(url, destfile, quiet) {
      downloads <<- downloads + 1L
      writeLines(paste0("download-", downloads), destfile)
    },
    .package = "stow"
  )
  url <- "https://example.com/files/data.csv"

  first <- stow(url, "testPackage", quiet = TRUE)
  etag <- '"second"'
  second <- stow(url, "testPackage", quiet = TRUE)

  expect_equal(downloads, 2L)
  expect_false(identical(first, second))
  expect_true(file.exists(first))
  expect_true(file.exists(second))
  expect_identical(readLines(first), "download-1")
  expect_identical(readLines(second), "download-2")
})

test_that("failed ETag probes fall back without blocking downloads", {
  local_stow_data_dir()
  downloads <- 0L
  testthat::local_mocked_bindings(
    .stow_probe_etag = function(url) stop("HEAD unsupported"),
    .stow_download_file = function(url, destfile, quiet) {
      downloads <<- downloads + 1L
      writeLines("downloaded", destfile)
    },
    .package = "stow"
  )
  url <- "https://example.com/files/data.csv"

  result <- stow(url, "testPackage", quiet = TRUE)
  expect_equal(downloads, 1L)
  expect_identical(result, normalizePath(stow_managed_copy_file(url), winslash = "/"))
})

test_that("overwrite replaces the matching managed local copy", {
  local_stow_data_dir()
  local_no_etag()
  downloads <- 0L
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) {
      downloads <<- downloads + 1L
      writeLines(paste0("version-", downloads), destfile)
    },
    .package = "stow"
  )
  url <- "https://example.com/files/data.csv"

  destination <- stow(url, "testPackage", quiet = TRUE)
  stow(url, "testPackage", overwrite = TRUE, quiet = TRUE)

  expect_equal(downloads, 2L)
  expect_identical(readLines(destination), "version-2")
  expect_length(
    list.files(dirname(destination), pattern = "backup", all.files = TRUE),
    0L
  )
})

test_that("offline lookup prefers the exact non-ETag path", {
  local_stow_data_dir()
  url <- "https://example.com/files/data.csv"
  exact <- stow_managed_copy_file(url)
  variant <- stow_managed_copy_file(url, etag = '"newer"')
  writeLines("exact", exact)
  writeLines("variant", variant)
  Sys.setFileTime(variant, Sys.time() + 60)
  testthat::local_mocked_bindings(
    .stow_probe_etag = function(...) stop("network metadata called"),
    .stow_download_file = function(...) stop("download called"),
    .package = "stow"
  )

  result <- stow(url, "testPackage", offline = TRUE, quiet = TRUE)
  expect_identical(result, normalizePath(exact, winslash = "/"))
})

test_that("offline lookup chooses newest variants with lexical ties", {
  local_stow_data_dir()
  url <- "https://example.com/files/data.csv"
  older <- stow_managed_copy_file(url, etag = '"older"')
  newer_a <- stow_managed_copy_file(url, etag = '"newer-a"')
  newer_b <- stow_managed_copy_file(url, etag = '"newer-b"')
  writeLines("older", older)
  writeLines("newer-a", newer_a)
  writeLines("newer-b", newer_b)
  now <- Sys.time()
  Sys.setFileTime(older, now - 60)
  Sys.setFileTime(c(newer_a, newer_b), now)

  result <- stow(url, "testPackage", offline = TRUE, quiet = TRUE)
  expected <- sort(c(newer_a, newer_b), method = "radix")[[1L]]
  expect_identical(result, normalizePath(expected, winslash = "/"))
})

test_that("offline mode rejects overwrite and never calls network helpers", {
  local_stow_data_dir()
  testthat::local_mocked_bindings(
    .stow_probe_etag = function(...) stop("network metadata called"),
    .stow_download_file = function(...) stop("download called"),
    .package = "stow"
  )
  url <- "https://example.com/files/data.csv"

  expect_error(
    stow(url, "testPackage", offline = TRUE, overwrite = TRUE),
    "cannot be combined"
  )
  expect_error(
    stow(url, "testPackage", offline = TRUE),
    "No managed local copy"
  )
})
