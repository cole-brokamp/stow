test_that("stow_path defaults to the stow cache and requires safe names", {
  local_stow_data_dir()

  expect_identical(stow_path(), normalizePath(
    tools::R_user_dir("stow", "data"),
    winslash = "/",
    mustWork = TRUE
  ))
  expect_error(stow_path(""), "one non-missing")
  expect_error(stow_path("../escape"), "path-safe")
  expect_error(stow_path("bad-name"), "path-safe")
  expect_error(stow_path("pkg", ""), "one non-empty")
  expect_error(stow_path("pkg", "/absolute"), "safe relative")
  expect_error(stow_path("pkg", "../escape"), "safe relative")
  expect_error(stow_path("pkg", "one//two"), "safe relative")
  expect_error(stow_path("pkg", ".hidden"), "safe relative")
})

test_that("stow_path honors R_USER_DATA_DIR and creates nested subdirectories", {
  root <- local_stow_data_dir()
  expected_root <- tools::R_user_dir("testPackage", "data")

  path <- stow_path("testPackage", "inputs/raw-data_2")
  expect_true(dir.exists(path))
  expect_identical(
    path,
    normalizePath(
      file.path(expected_root, "inputs", "raw-data_2"),
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_true(startsWith(path, root))

  backslash_path <- stow_path("testPackage", "other\\nested")
  expect_true(dir.exists(backslash_path))
  expect_true(endsWith(backslash_path, "/other/nested"))
})

test_that("stow_info returns a typed empty data frame", {
  local_stow_data_dir()

  info <- stow_info()
  expect_s3_class(info, "data.frame")
  expect_identical(names(info), c("path", "size", "modified"))
  expect_equal(nrow(info), 0L)
  expect_type(info$path, "character")
  expect_type(info$size, "double")
  expect_s3_class(info$modified, "POSIXct")
})

test_that("stow_info lists regular files recursively", {
  local_stow_data_dir()
  root <- stow_path("testPackage")
  dir.create(file.path(root, "nested"))
  writeBin(charToRaw("abc"), file.path(root, "one.bin"))
  writeBin(charToRaw("12345"), file.path(root, "nested", "two.bin"))

  info <- stow_info("testPackage")
  expect_s3_class(info, "data.frame")
  expect_equal(nrow(info), 2L)
  expect_setequal(basename(info$path), c("one.bin", "two.bin"))
  expect_setequal(info$size, c(3, 5))
  expect_true(all(startsWith(info$path, root)))
})
