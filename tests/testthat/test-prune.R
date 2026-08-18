test_that("stow_prune returns a typed empty report", {
  local_stow_data_dir()

  expect_message(report <- stow_prune(), "No cache files")
  expect_s3_class(report, "data.frame")
  expect_identical(
    names(report),
    c("path", "type", "modified", "removed")
  )
  expect_equal(nrow(report), 0L)
  expect_type(report$path, "character")
  expect_type(report$type, "character")
  expect_s3_class(report$modified, "POSIXct")
  expect_type(report$removed, "logical")
})

test_that("stow_prune retains current entries and removes aged variants", {
  local_stow_data_dir()
  now <- as.POSIXct("2026-08-18 12:00:00", tz = "UTC")
  testthat::local_mocked_bindings(
    .stow_now = function() now,
    .package = "stow"
  )
  url <- "https://example.com/files/data.csv"
  exact <- stow_cache_file(url)
  oldest <- stow_cache_file(url, etag = '"oldest"')
  protected <- stow_cache_file(url, etag = '"protected"')
  newest <- stow_cache_file(url, etag = '"newest"')
  writeLines("exact", exact)
  writeLines("oldest", oldest)
  writeLines("protected", protected)
  writeLines("newest", newest)
  Sys.setFileTime(exact, now - 100 * 86400)
  Sys.setFileTime(oldest, now - 90 * 86400)
  Sys.setFileTime(protected, now - 60 * 86400)
  Sys.setFileTime(newest, now - 40 * 86400)

  expect_message(
    preview <- stow_prune(
      url,
      "testPackage",
      max_age = 30,
      keep = protected,
      dry_run = TRUE
    ),
    "1 cache file is eligible"
  )
  expect_identical(preview$path, normalizePath(oldest, winslash = "/"))
  expect_identical(preview$type, "etag_variant")
  expect_true(is.na(preview$removed))
  expect_true(all(file.exists(c(exact, oldest, protected, newest))))

  expect_message(
    removed <- stow_prune(
      url,
      "testPackage",
      max_age = 30,
      keep = protected
    ),
    "Removed 1 of 1"
  )
  expect_true(removed$removed)
  expect_false(file.exists(oldest))
  expect_true(all(file.exists(c(exact, protected, newest))))
})

test_that("stow_prune protects the deterministic newest ETag fallback", {
  local_stow_data_dir()
  now <- as.POSIXct("2026-08-18 12:00:00", tz = "UTC")
  testthat::local_mocked_bindings(
    .stow_now = function() now,
    .package = "stow"
  )
  url <- "https://example.com/files/archive.tar.gz"
  first <- stow_cache_file(url, etag = '"first"')
  second <- stow_cache_file(url, etag = '"second"')
  writeLines("first", first)
  writeLines("second", second)
  Sys.setFileTime(c(first, second), now - 90 * 86400)
  expected <- sort(c(first, second), method = "radix")[[1L]]
  removed_path <- normalizePath(
    setdiff(c(first, second), expected),
    winslash = "/"
  )

  report <- stow_prune(
    url,
    "testPackage",
    max_age = 30,
    quiet = TRUE
  )

  expect_identical(
    report$path,
    removed_path
  )
  expect_true(file.exists(expected))
  expect_false(file.exists(report$path))
  expect_identical(
    stow(url, "testPackage", offline = TRUE, quiet = TRUE),
    normalizePath(expected, winslash = "/")
  )
})

test_that("stow_prune retains recent variants and ignores them without a URL", {
  local_stow_data_dir()
  now <- as.POSIXct("2026-08-18 12:00:00", tz = "UTC")
  testthat::local_mocked_bindings(
    .stow_now = function() now,
    .package = "stow"
  )
  url <- "https://example.com/files/data.csv"
  old <- stow_cache_file(url, etag = '"old"')
  recent <- stow_cache_file(url, etag = '"recent"')
  writeLines("old", old)
  writeLines("recent", recent)
  Sys.setFileTime(old, now - 60 * 86400)
  Sys.setFileTime(recent, now - 1 * 86400)

  expect_silent(expect_equal(stow_prune(quiet = TRUE), data.frame(
    path = character(),
    type = character(),
    modified = as.POSIXct(character()),
    removed = logical()
  )))
  expect_true(file.exists(old))

  report <- stow_prune(
    url,
    "testPackage",
    max_age = 90,
    quiet = TRUE
  )
  expect_equal(nrow(report), 0L)
  expect_true(all(file.exists(c(old, recent))))
})

test_that("stow_prune cleans only aged orphaned internal files", {
  local_stow_data_dir()
  now <- as.POSIXct("2026-08-18 12:00:00", tz = "UTC")
  testthat::local_mocked_bindings(
    .stow_now = function() now,
    .package = "stow"
  )
  directory <- stow_path("testPackage")
  nested <- file.path(directory, "nested")
  dir.create(nested)
  destination <- stow_cache_file("https://example.com/files/data.csv")
  writeLines("current", destination)

  old_temp <- file.path(directory, ".stow-download-ab12")
  nested_temp <- file.path(nested, ".stow-download-cd34")
  recent_temp <- file.path(directory, ".stow-download-ef56")
  false_temp <- file.path(directory, ".stow-download-ab12.txt")
  backup <- file.path(
    directory,
    paste0(".", basename(destination), ".backup-ab12")
  )
  recovery <- file.path(
    directory,
    paste0(".", basename(destination), "-missing.backup-ab12")
  )
  false_backup <- file.path(directory, ".notes.backup-ab12")
  for (path in c(
    old_temp,
    nested_temp,
    recent_temp,
    false_temp,
    backup,
    recovery,
    false_backup
  )) {
    writeLines("artifact", path)
  }
  temp_directory <- file.path(directory, ".stow-download-dir12")
  dir.create(temp_directory)
  Sys.setFileTime(
    c(
      old_temp,
      nested_temp,
      false_temp,
      backup,
      recovery,
      false_backup,
      temp_directory
    ),
    now - 60 * 86400
  )
  Sys.setFileTime(recent_temp, now - 1 * 86400)
  expected_removed <- normalizePath(
    c(old_temp, nested_temp, backup),
    winslash = "/"
  )

  report <- stow_prune(
    package = "testPackage",
    max_age = 30,
    quiet = TRUE
  )

  expect_setequal(
    report$path,
    expected_removed
  )
  expect_setequal(report$type, c("temporary", "temporary", "backup"))
  expect_true(all(report$removed))
  expect_false(any(file.exists(c(old_temp, nested_temp, backup))))
  expect_true(all(file.exists(c(
    destination,
    recent_temp,
    false_temp,
    recovery,
    false_backup,
    temp_directory
  ))))
})

test_that("stow_prune respects package and subdirectory boundaries", {
  local_stow_data_dir()
  now <- as.POSIXct("2026-08-18 12:00:00", tz = "UTC")
  testthat::local_mocked_bindings(
    .stow_now = function() now,
    .package = "stow"
  )
  default_temp <- file.path(stow_path(), ".stow-download-default1")
  other_temp <- file.path(stow_path("other-project"), ".stow-download-other1")
  nested_temp <- file.path(
    stow_path("other-project", "inputs"),
    ".stow-download-nested1"
  )
  writeLines("temp", default_temp)
  writeLines("temp", other_temp)
  writeLines("temp", nested_temp)
  Sys.setFileTime(c(default_temp, other_temp, nested_temp), now - 60 * 86400)
  expected_removed <- normalizePath(nested_temp, winslash = "/")

  report <- stow_prune(
    package = "other-project",
    subdir = "inputs",
    max_age = 30,
    quiet = TRUE
  )

  expect_identical(
    report$path,
    expected_removed
  )
  expect_true(all(file.exists(c(default_temp, other_temp))))
})

test_that("stow_remove evicts only entries for the selected URL", {
  local_stow_data_dir()
  url <- "https://example.com/files/data.csv"
  other_url <- "https://other.example.com/files/data.csv"
  exact <- stow_cache_file(url)
  variant_a <- stow_cache_file(url, etag = '"a"')
  variant_b <- stow_cache_file(url, etag = '"b"')
  other <- stow_cache_file(other_url)
  temporary <- file.path(stow_path("testPackage"), ".stow-download-ab12")
  for (path in c(exact, variant_a, variant_b, other, temporary)) {
    writeLines("cached", path)
  }

  preview <- stow_remove(
    url,
    "testPackage",
    dry_run = TRUE,
    quiet = TRUE
  )
  expect_setequal(
    preview$path,
    normalizePath(c(exact, variant_a, variant_b), winslash = "/")
  )
  expect_setequal(
    preview$type,
    c("cache_entry", "etag_variant", "etag_variant")
  )
  expect_true(all(is.na(preview$removed)))
  expect_true(all(file.exists(c(exact, variant_a, variant_b))))

  report <- stow_remove(url, "testPackage", quiet = TRUE)
  expect_true(all(report$removed))
  expect_false(any(file.exists(c(exact, variant_a, variant_b))))
  expect_true(all(file.exists(c(other, temporary))))
})

test_that("stow_remove never removes a directory at a cache path", {
  local_stow_data_dir()
  url <- "https://example.com/files/data.csv"
  exact <- stow_cache_file(url)
  dir.create(exact)

  report <- stow_remove(url, "testPackage", quiet = TRUE)

  expect_equal(nrow(report), 0L)
  expect_true(dir.exists(exact))
})

test_that("failed removals are reported without claiming success", {
  local_stow_data_dir()
  now <- as.POSIXct("2026-08-18 12:00:00", tz = "UTC")
  testthat::local_mocked_bindings(
    .stow_now = function() now,
    .stow_unlink = function(path) 1L,
    .package = "stow"
  )
  temporary <- file.path(stow_path(), ".stow-download-ab12")
  writeLines("temp", temporary)
  Sys.setFileTime(temporary, now - 60 * 86400)

  expect_warning(
    report <- stow_prune(max_age = 30, quiet = TRUE),
    "Could not remove 1 cache file"
  )
  expect_false(report$removed)
  expect_true(file.exists(temporary))
})

test_that("cache lifecycle arguments are validated", {
  local_stow_data_dir()
  url <- "https://example.com/files/data.csv"

  expect_error(stow_prune(max_age = -1), "non-negative")
  expect_error(stow_prune(max_age = Inf), "finite")
  expect_error(stow_prune(max_age = c(1, 2)), "one finite")
  expect_error(stow_prune(dry_run = NA), "`dry_run` must be")
  expect_error(stow_prune(quiet = 1), "`quiet` must be")
  expect_error(stow_prune(keep = "anything"), "only when `url`")
  expect_error(stow_prune(url, keep = character()), "one or more")
  expect_error(stow_prune(url, keep = tempfile()), "existing cache entry")
  unrelated <- stow_cache_file("https://other.example.com/files/data.csv")
  writeLines("unrelated", unrelated)
  expect_error(stow_prune(url, keep = unrelated), "cache entry for `url`")
  expect_error(stow_remove(), "`url` is required", fixed = TRUE)
  expect_error(stow_remove(url, dry_run = NA), "`dry_run` must be")
  expect_error(stow_remove(url, quiet = 1), "`quiet` must be")
})
