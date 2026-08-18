test_that("URL-derived names are stable and preserve filenames", {
  url <- "https://example.com/a/b/archive.tar.gz"
  expected_hash <- digest::digest(
    dirname(url),
    algo = "xxhash64",
    serialize = FALSE
  )
  expect_match(expected_hash, "^[0-9a-f]{16}$")

  expect_identical(
    .stow_url_to_filename(url),
    paste0(expected_hash, "--archive.tar.gz")
  )
  expect_identical(
    .stow_url_to_filename("https://example.com/files/README"),
    paste0(
      digest::digest(
        "https://example.com/files",
        algo = "xxhash64",
        serialize = FALSE
      ),
      "--README"
    )
  )
})

test_that("ETags are normalized, hashed, and inserted before extensions", {
  url <- "https://example.com/a/data.csv"
  normalized_hash <- digest::digest(
    "private-tag",
    algo = "xxhash64",
    serialize = FALSE
  )
  expect_match(normalized_hash, "^[0-9a-f]{16}$")
  name <- .stow_url_to_filename(url, 'W/"private-tag"')

  expect_match(name, paste0("--", normalized_hash, "\\.csv$"))
  expect_false(grepl("private-tag", name, fixed = TRUE))
  expect_false(grepl("W/", name, fixed = TRUE))
  expect_identical(
    name,
    .stow_url_to_filename(url, '"private-tag"')
  )

  extensionless <- .stow_url_to_filename(
    "https://example.com/a/README",
    '"private-tag"'
  )
  expect_match(extensionless, paste0("--README--", normalized_hash, "$"))
})

test_that("URL validation enforces supported, filename-bearing sources", {
  expect_error(.stow_url_to_filename("file:///tmp/data.csv"), "scheme")
  expect_error(.stow_url_to_filename("https://example.com"), "filename")
  expect_error(.stow_url_to_filename("https://example.com/"), "filename")
  expect_error(
    .stow_url_to_filename("https://example.com/data.csv?raw=1"),
    "query strings"
  )
  expect_error(
    .stow_url_to_filename("https://example.com/data.csv#section"),
    "fragments"
  )
  expect_error(
    .stow_url_to_filename("https://user@example.com/data.csv"),
    "embedded credentials"
  )
  expect_no_error(.stow_url_to_filename("ftp://example.com/data.csv"))
  expect_no_error(.stow_url_to_filename("ftps://example.com/data.csv"))
})
