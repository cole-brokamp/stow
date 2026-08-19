#' Locate or create the managed local copy directory
#'
#' `stow_path()` creates and returns the directory where managed local copies
#' downloaded by [stow()] are saved. The directory is a fixed `stow`
#' subdirectory of the platform-appropriate, package-specific user data
#' location returned by [tools::R_user_dir()] with `which = "data"`, so it
#' remains available across R sessions and is not treated as disposable.
#'
#' @inheritParams stow
#'
#' @section Environment variables:
#' `R_USER_DATA_DIR` and `XDG_DATA_HOME` are environment variables, not R
#' options. [tools::R_user_dir()] first uses `R_USER_DATA_DIR`; if it is unset,
#' it uses `XDG_DATA_HOME` when available, followed by the platform-specific
#' default. `stow_path()` creates the package-specific data directory, a fixed
#' `stow` directory beneath it, and any requested `subdir`.
#'
#' Environment variables can be set before R starts, either through the
#' operating system environment or with a line in a user or project
#' `.Renviron` file:
#'
#' ```
#' R_USER_DATA_DIR=/path/to/data
#' ```
#'
#' They can also be set after R has started with [Sys.setenv()]:
#'
#' ```r
#' Sys.setenv(R_USER_DATA_DIR = "/path/to/data")
#' stow_path()
#' ```
#'
#' A value set with [Sys.setenv()] affects subsequent calls in the current R
#' process; it does not move files that were already downloaded. Use
#' `withr::with_envvar()` for a temporary, scoped change (see examples).
#'
#' @return The absolute directory path for managed local copies as a character
#'   scalar. The directory is created if it does not already exist.
#' @seealso [stow()] to download a managed local copy, [stow_info()] to list
#'   managed local copies, and [stow_prune()] to manage retained copies.
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

  path <- file.path(
    tools::R_user_dir(package = package, which = "data"),
    "stow"
  )
  if (length(parts) > 0L) {
    path <- do.call(file.path, as.list(c(path, parts)))
  }

  if (!dir.exists(path)) {
    created <- dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!isTRUE(created) && !dir.exists(path)) {
      stop(
        "Could not create the managed local copy directory:\n",
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
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$", package)) {
    stop(
      paste0(
        "`package` must be a valid R package name: at least two characters, ",
        "beginning with an ASCII letter, containing only ASCII letters, ",
        "numbers, and periods, and ending with a letter or number."
      ),
      call. = FALSE
    )
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
        "`subdir` must be a relative path without empty, `.`, or `..` ",
        "components. Each component must begin with a letter or number, ",
        "contain only letters, numbers, dots, underscores, and hyphens, ",
        "and not end in a dot."
      ),
      call. = FALSE
    )
  }
  parts
}
