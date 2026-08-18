#' Prune outdated and orphaned cache files
#'
#' `stow_prune()` actively manages files created by [stow()] without deleting
#' the current offline fallback for a URL. It makes no network requests and
#' never removes directories or unrecognized files.
#'
#' @param url `NULL` or one URL accepted by [stow()]. When supplied, outdated
#'   ETag variants for that URL are eligible for removal. With `url = NULL`,
#'   only orphaned stow temporary and backup files are considered.
#' @inheritParams stow
#' @param max_age A non-negative number of days. A file must be at least this
#'   old to be eligible for removal. The default retains eligible files for 30
#'   days. Use `0` to remove eligible files regardless of age.
#' @param keep `NULL` or cached paths for `url` that must be retained in
#'   addition to the protections applied automatically. A path returned by a
#'   recent `stow(url, ...)` call can be supplied when the server has returned
#'   to an older ETag version. Each path must be an existing cache entry for
#'   `url` in the selected namespace and subdirectory.
#' @param dry_run Whether to report eligible files without removing them.
#' @param quiet Whether to suppress informational cleanup messages. Warnings
#'   and errors are never suppressed.
#'
#' @section Retention and removal rules:
#' For a supplied `url`, the URL-derived non-ETag entry is always retained.
#' The newest ETag variant by modification time is also always retained, using
#' lexical filename order to break ties. This is the same ETag fallback that
#' [stow()] would select in offline mode when no non-ETag entry exists. Paths
#' supplied through `keep` receive an additional explicit protection. Other
#' ETag variants are removed only after they reach `max_age`.
#'
#' Temporary files whose names begin with `.stow-download-` are incomplete
#' staged downloads and become eligible after `max_age`. Replacement backups
#' become eligible only when the corresponding destination exists. A backup
#' whose destination is missing is retained because it may be the only
#' recoverable copy. Orphan cleanup is recursive within the selected cache
#' namespace or `subdir`.
#'
#' Use [stow_remove()] when the intent is to evict every cache entry for a
#' known URL, including its current usable entry.
#'
#' @return A base data frame describing files eligible for removal, with
#'   absolute `path`, `type`, last-modified time in `modified`, and logical
#'   `removed`. `removed` is `NA` during a dry run, `TRUE` after successful
#'   removal, and `FALSE` if removal failed. If no files are eligible, a
#'   zero-row data frame with the same columns is returned.
#' @seealso [stow_remove()] to evict all entries for a URL, [stow_info()] to
#'   inspect a cache, and [stow_path()] to locate it.
#' @export
#'
#' @examples
#' withr::with_envvar(c(R_USER_DATA_DIR = tempfile("stow-data-")), {
#'   stow_prune(dry_run = TRUE)
#' })
stow_prune <- function(
  url = NULL,
  package = "stow",
  subdir = NULL,
  max_age = 30,
  keep = NULL,
  dry_run = FALSE,
  quiet = FALSE
) {
  package <- .stow_check_package(package)
  .stow_check_flag(dry_run, "dry_run")
  .stow_check_flag(quiet, "quiet")
  max_age <- .stow_check_max_age(max_age)

  if (is.null(url)) {
    if (!is.null(keep)) {
      stop("`keep` can be used only when `url` is supplied.", call. = FALSE)
    }
  } else {
    url <- .stow_url_details(url)$url
  }

  directory <- stow_path(package = package, subdir = subdir)
  cutoff <- .stow_now() - as.difftime(max_age, units = "days")
  candidates <- .stow_orphan_candidates(directory, cutoff)

  if (!is.null(url)) {
    exact <- file.path(directory, .stow_url_to_filename(url))
    variants <- .stow_variant_candidates(exact)
    protected <- .stow_protected_paths(exact, variants, keep)
    variants <- setdiff(variants, protected)
    variants <- .stow_old_enough(variants, cutoff)
    candidates <- rbind(
      candidates,
      .stow_candidate_frame(variants, "etag_variant")
    )
  }

  candidates <- .stow_order_candidates(candidates)
  .stow_remove_candidates(candidates, dry_run = dry_run, quiet = quiet)
}

#' Remove every cache entry for a URL
#'
#' `stow_remove()` explicitly evicts all regular cache files belonging to one
#' URL in a selected namespace and subdirectory. This includes the non-ETag
#' entry and every ETag variant, so a later [stow()] call must download the
#' file again. It makes no network requests and does not remove directories,
#' internal temporary or backup files, entries for other URLs, or unrecognized
#' files.
#'
#' @inheritParams stow
#' @param dry_run Whether to report matching cache entries without removing
#'   them.
#' @param quiet Whether to suppress informational removal messages. Warnings
#'   and errors are never suppressed.
#'
#' @return A base data frame describing matching cache entries, with absolute
#'   `path`, `type`, last-modified time in `modified`, and logical `removed`.
#'   `removed` is `NA` during a dry run, `TRUE` after successful removal, and
#'   `FALSE` if removal failed. If no entries match, a zero-row data frame with
#'   the same columns is returned.
#' @seealso [stow_prune()] for conservative cleanup that retains current usable
#'   entries, and [stow_info()] to inspect a cache.
#' @export
#'
#' @examples
#' withr::with_envvar(c(R_USER_DATA_DIR = tempfile("stow-data-")), {
#'   stow_remove(
#'     "https://example.com/files/data.csv",
#'     dry_run = TRUE
#'   )
#' })
stow_remove <- function(
  url,
  package = "stow",
  subdir = NULL,
  dry_run = FALSE,
  quiet = FALSE
) {
  if (missing(url)) {
    stop("`url` is required.", call. = FALSE)
  }
  url <- .stow_url_details(url)$url
  package <- .stow_check_package(package)
  .stow_check_flag(dry_run, "dry_run")
  .stow_check_flag(quiet, "quiet")

  directory <- stow_path(package = package, subdir = subdir)
  exact <- file.path(directory, .stow_url_to_filename(url))
  variants <- .stow_variant_candidates(exact)
  exact <- exact[.stow_is_regular_file(exact)]
  candidates <- rbind(
    .stow_candidate_frame(exact, "cache_entry"),
    .stow_candidate_frame(variants, "etag_variant")
  )
  candidates <- .stow_order_candidates(candidates)
  .stow_remove_candidates(candidates, dry_run = dry_run, quiet = quiet)
}

.stow_check_max_age <- function(max_age) {
  if (
    !is.numeric(max_age) ||
      length(max_age) != 1L ||
      is.na(max_age) ||
      !is.finite(max_age) ||
      max_age < 0
  ) {
    stop("`max_age` must be one finite, non-negative number.", call. = FALSE)
  }
  as.numeric(max_age)
}

.stow_now <- function() {
  Sys.time()
}

.stow_protected_paths <- function(exact, variants, keep) {
  protected <- exact
  newest <- .stow_newest_variant(exact)
  if (!is.na(newest)) {
    protected <- c(protected, newest)
  }
  if (is.null(keep)) {
    return(unique(protected))
  }
  if (
    !is.character(keep) ||
      length(keep) == 0L ||
      anyNA(keep) ||
      any(!nzchar(keep))
  ) {
    stop(
      "`keep` must be NULL or one or more non-missing cache paths.",
      call. = FALSE
    )
  }

  regular_keep <- vapply(keep, .stow_is_regular_file, logical(1))
  if (any(!regular_keep)) {
    stop(
      "Every `keep` path must be an existing cache entry for `url`.",
      call. = FALSE
    )
  }
  allowed <- c(exact[.stow_is_regular_file(exact)], variants)
  allowed <- normalizePath(allowed, winslash = "/", mustWork = TRUE)
  keep <- vapply(
    keep,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = TRUE,
    USE.NAMES = FALSE
  )
  if (any(!keep %in% allowed)) {
    stop(
      "Every `keep` path must be an existing cache entry for `url`.",
      call. = FALSE
    )
  }
  unique(c(protected, keep))
}

.stow_orphan_candidates <- function(directory, cutoff) {
  paths <- list.files(
    directory,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  if (length(paths) == 0L) {
    return(.stow_empty_prune_info())
  }
  info <- file.info(paths)
  paths <- paths[!is.na(info$isdir) & !info$isdir]
  names <- basename(paths)

  temporary <- grepl("^\\.stow-download-[[:alnum:]]+$", names)
  backup_destination <- .stow_backup_destination(paths)
  backup <- !is.na(backup_destination) & vapply(
    backup_destination,
    .stow_is_regular_file,
    logical(1)
  )

  temporary_paths <- .stow_old_enough(paths[temporary], cutoff)
  backup_paths <- .stow_old_enough(paths[backup], cutoff)
  rbind(
    .stow_candidate_frame(temporary_paths, "temporary"),
    .stow_candidate_frame(backup_paths, "backup")
  )
}

.stow_backup_destination <- function(paths) {
  destinations <- rep(NA_character_, length(paths))
  names <- basename(paths)
  matches <- regexec("^\\.(.+)\\.backup-[[:alnum:]]+$", names)
  pieces <- regmatches(names, matches)
  for (index in seq_along(pieces)) {
    if (length(pieces[[index]]) != 2L) {
      next
    }
    destination_name <- pieces[[index]][[2L]]
    if (!grepl("^[0-9a-f]{16}--.+", destination_name)) {
      next
    }
    destinations[[index]] <- file.path(dirname(paths[[index]]), destination_name)
  }
  destinations
}

.stow_old_enough <- function(paths, cutoff) {
  if (length(paths) == 0L) {
    return(character())
  }
  modified <- file.info(paths)$mtime
  paths[!is.na(modified) & modified <= cutoff]
}

.stow_candidate_frame <- function(paths, type) {
  if (length(paths) == 0L) {
    return(.stow_empty_prune_info())
  }
  data.frame(
    path = normalizePath(paths, winslash = "/", mustWork = TRUE),
    type = rep(type, length(paths)),
    modified = as.POSIXct(file.info(paths)$mtime),
    removed = rep(NA, length(paths)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

.stow_empty_prune_info <- function() {
  data.frame(
    path = character(),
    type = character(),
    modified = as.POSIXct(character()),
    removed = logical(),
    stringsAsFactors = FALSE
  )
}

.stow_order_candidates <- function(candidates) {
  if (nrow(candidates) == 0L) {
    return(candidates)
  }
  candidates[order(candidates$path, method = "radix"), , drop = FALSE]
}

.stow_remove_candidates <- function(candidates, dry_run, quiet) {
  if (nrow(candidates) == 0L) {
    if (!quiet) {
      message("No cache files were eligible for removal.")
    }
    return(candidates)
  }
  if (dry_run) {
    if (!quiet) {
      message(
        nrow(candidates),
        if (nrow(candidates) == 1L) " cache file is" else " cache files are",
        " eligible for removal."
      )
    }
    return(candidates)
  }

  removed <- vapply(
    candidates$path,
    function(path) {
      result <- .stow_unlink(path)
      identical(result, 0L) || !file.exists(path)
    },
    logical(1)
  )
  candidates$removed <- removed
  if (any(!removed)) {
    warning(
      "Could not remove ",
      sum(!removed),
      if (sum(!removed) == 1L) " cache file." else " cache files.",
      call. = FALSE
    )
  }
  if (!quiet) {
    message(
      "Removed ",
      sum(removed),
      " of ",
      nrow(candidates),
      if (nrow(candidates) == 1L) " eligible cache file." else " eligible cache files."
    )
  }
  candidates
}
