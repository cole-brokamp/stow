#' Locate a stow directory
#'
#' `stow_path()` creates and returns a durable package data directory. Set
#' `R_USER_DATA_DIR`, or another variable honored by [tools::R_user_dir()], to
#' relocate package data.
#'
#' @param package A non-empty package name. The default, `"stow"`, selects the
#'   shared cache used by direct calls to [stow()].
#' @param subdir An optional safe relative subdirectory. A scalar path with
#'   one or more components is accepted; each component may contain letters,
#'   numbers, dots, underscores, and hyphens.
#'
#' @return An absolute character path of length one.
#' @export
#'
#' @examples
#' withr::with_envvar(
#'   c(R_USER_DATA_DIR = tempfile("stow-data-")),
#'   stow_path(subdir = "inputs/raw")
#' )
stow_path <- function(package = "stow", subdir = NULL) {
  package <- .stow_check_package(package)
  parts <- .stow_subdir_parts(subdir)

  path <- tools::R_user_dir(package = package, which = "data")
  if (length(parts) > 0L) {
    path <- do.call(file.path, as.list(c(path, parts)))
  }

  if (!dir.exists(path)) {
    created <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(created) && !dir.exists(path)) {
      stop(
        "Could not create the package data directory:\n",
        path,
        call. = FALSE
      )
    }
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.stow_check_package <- function(package) {
  if (
    !is.character(package) ||
      length(package) != 1L ||
      is.na(package) ||
      !nzchar(package)
  ) {
    stop("`package` must be one non-missing character string.", call. = FALSE)
  }
  if (
    !grepl("^[A-Za-z][A-Za-z0-9.]*$", package) ||
      endsWith(package, ".")
  ) {
    stop("`package` must be a valid, path-safe R package name.", call. = FALSE)
  }
  package
}

.stow_subdir_parts <- function(subdir) {
  if (is.null(subdir)) {
    return(character())
  }
  if (
    !is.character(subdir) ||
      length(subdir) != 1L ||
      is.na(subdir) ||
      !nzchar(subdir)
  ) {
    stop(
      "`subdir` must be NULL or one non-empty character string.",
      call. = FALSE
    )
  }

  subdir <- gsub("\\\\", "/", subdir)
  parts <- strsplit(subdir, "/", fixed = TRUE)[[1L]]
  safe <-
    length(parts) > 0L &&
    all(nzchar(parts)) &&
    all(parts != ".") &&
    all(parts != "..") &&
    all(grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", parts)) &&
    all(!endsWith(parts, "."))
  if (!safe) {
    stop(
      paste0(
        "`subdir` must be a safe relative path whose components use only ",
        "letters, numbers, dots, underscores, and hyphens."
      ),
      call. = FALSE
    )
  }
  parts
}
