test_that("stow validates required and scalar arguments", {
  local_stow_data_dir()
  url <- "https://example.com/files/data.csv"

  expect_error(stow(package = "testPackage"), "`url` is required", fixed = TRUE)
  expect_error(stow(url, "testPackage", overwrite = NA), "`overwrite` must be")
  expect_error(stow(url, "testPackage", offline = 1), "`offline` must be")
  expect_error(stow(url, "testPackage", quiet = logical()), "`quiet` must be")
  expect_error(stow(url, "testPackage", etag = c(TRUE, FALSE)), "`etag` must be")
})

test_that("stow returns a visible absolute character scalar", {
  local_stow_data_dir()
  local_no_etag()
  testthat::local_mocked_bindings(
    .stow_download_file = function(url, destfile, quiet) writeLines("ok", destfile),
    .package = "stow"
  )

  visible <- withVisible(
    stow("https://example.com/files/data.csv", quiet = TRUE)
  )
  expect_true(visible$visible)
  expect_type(visible$value, "character")
  expect_length(visible$value, 1L)
  expect_match(visible$value, "^(/|[A-Za-z]:/)")
  expect_identical(dirname(visible$value), stow_path())
})
