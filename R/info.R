#' List files in a cache directory
#'
#' `stow_info()` lists regular files recursively below [stow_path()]. It
#' reports storage metadata only: it does not read the files, apply a
#' validator, or make network requests.
#'
#' @inheritParams stow
#'
#' @return A base data frame with the absolute file `path`, `size` in bytes,
#'   and last-modified time in `modified`. An empty cache returns a zero-row
#'   data frame with the same columns.
#' @seealso [stow()] to download a file, [stow_path()] to locate its cache
#'   directory, and [stow_prune()] to manage retained cache files.
#' @export
#'
#' @examples
#' withr::with_envvar(
#'   c(R_USER_DATA_DIR = tempfile("stow-data-")),
#'   stow_info()
#' )
stow_info <- function(package = "stow", subdir = NULL) {
  root <- stow_path(package = package, subdir = subdir)
  paths <- list.files(
    root,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )

  if (length(paths) == 0L) {
    return(.stow_empty_info())
  }

  info <- file.info(paths)
  keep <- !is.na(info$isdir) & !info$isdir
  if (!any(keep)) {
    return(.stow_empty_info())
  }

  paths <- paths[keep]
  info <- info[keep, , drop = FALSE]
  data.frame(
    path = normalizePath(paths, winslash = "/", mustWork = TRUE),
    size = unname(info$size),
    modified = as.POSIXct(info$mtime),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

.stow_empty_info <- function() {
  data.frame(
    path = character(),
    size = numeric(),
    modified = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
}
