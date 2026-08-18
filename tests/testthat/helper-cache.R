local_stow_data_dir <- function(.local_envir = parent.frame()) {
  root <- tempfile("stow-test-data-")
  dir.create(root)
  withr::local_envvar(
    c(R_USER_DATA_DIR = root),
    .local_envir = .local_envir
  )
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

stow_cache_file <- function(url, package = "testPackage", etag = NULL) {
  file.path(
    stow_path(package),
    .stow_url_to_filename(url, etag = etag)
  )
}

local_no_etag <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    .stow_probe_etag = function(url) NA_character_,
    .package = "stow",
    .env = .local_envir
  )
}
