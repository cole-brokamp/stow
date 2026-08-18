#' Download and cache a file
#'
#' `stow()` downloads a source file into the durable data directory returned by
#' [stow_path()]. Files are cached to the data directory and named by combining
#' a hash of the URL directory with the URL basename.
#' When available, a hash of the normalized ETag is
#' inserted before the extension.
#'
#' Online calls use an existing matching cache entry unless `overwrite` is
#' `TRUE`. If an ETag probe fails or is unsupported, downloading continues with
#' the non-ETag cache name. Offline calls make no network requests: they prefer
#' the non-ETag cache entry, then choose the newest matching ETag variant (with
#' lexical ordering as the deterministic timestamp tie-breaker).
#'
#' @param url A scalar `https`, `http`, `ftp`, or `ftps` URL ending in a
#'   filename. Query strings and fragments are not supported.
#' @param package A non-empty cache namespace used by [tools::R_user_dir()]. It
#'   must begin with a letter and may contain letters, numbers, dots,
#'   underscores, and hyphens. The default, `"stow"`, provides a shared cache
#'   for direct use. Package/project authors can supply their package or project
#'   name to use a separate cache.
#' @param subdir An optional relative path below the package data directory.
#'   Each component must begin with a letter or number, may otherwise contain
#'   letters, numbers, dots, underscores, or hyphens, and must not end in a
#'   dot. Absolute paths, empty components, `.` components, and `..` components
#'   are rejected.
#' @param overwrite Whether to replace an existing matching cache entry.
#' @param offline Whether to prohibit all network operations and use only a
#'   cached file.
#' @param quiet Whether to suppress informational messages and download
#'   progress. Warnings and errors are never suppressed.
#' @param etag Whether online calls should probe for an ETag and include its
#'   hash in the cache name.
#' @param validate An optional function called with one file path. It must
#'   return exactly `TRUE`; an error or any other result marks the file invalid.
#'
#' @return The absolute cached-file path as a visible character scalar.
#' @export
#'
#' @examples
#' \donttest{
#' withr::with_envvar(
#'   c(R_USER_DATA_DIR = tempdir()),
#'   stow(
#'     "https://github.com/geomarker-io/addr/releases/download/v1.3.0/addr-taf-v1-2025.json"
#'   )
#' )
#' }
stow <- function(
  url,
  package = "stow",
  subdir = NULL,
  overwrite = FALSE,
  offline = FALSE,
  quiet = FALSE,
  etag = TRUE,
  validate = NULL
) {
  if (missing(url)) {
    stop("`url` is required.", call. = FALSE)
  }
  url <- .stow_url_details(url)$url
  package <- .stow_check_package(package)
  .stow_check_flag(overwrite, "overwrite")
  .stow_check_flag(offline, "offline")
  .stow_check_flag(quiet, "quiet")
  .stow_check_flag(etag, "etag")
  if (!is.null(validate) && !is.function(validate)) {
    stop("`validate` must be NULL or a function.", call. = FALSE)
  }
  if (offline && overwrite) {
    stop(
      "`offline = TRUE` cannot be combined with `overwrite = TRUE`.",
      call. = FALSE
    )
  }

  directory <- stow_path(package = package, subdir = subdir)
  exact <- file.path(directory, .stow_url_to_filename(url))
  if (offline) {
    return(.stow_offline(url, exact, quiet, validate))
  }

  etag_value <- NA_character_
  if (etag) {
    etag_value <- suppressWarnings(
      tryCatch(.stow_probe_etag(url), error = function(error) NA_character_)
    )
  }
  destination <- file.path(
    directory,
    .stow_url_to_filename(url, etag = etag_value)
  )

  replace_existing <- overwrite
  if (file.exists(destination)) {
    if (dir.exists(destination)) {
      stop(
        "The expected file path is occupied by a directory:\n",
        destination,
        call. = FALSE
      )
    }
    if (!overwrite) {
      validation <- .stow_validate_file(destination, validate)
      if (validation$valid) {
        .stow_cache_message(destination, quiet)
        return(normalizePath(destination, winslash = "/", mustWork = TRUE))
      }
      replace_existing <- TRUE
      if (!quiet) {
        message(
          "Cached file failed validation; downloading a replacement:\n  ",
          destination
        )
      }
    }
  }

  if (!quiet) {
    message("Downloading ", url, " -> ", destination)
  }
  temp <- tempfile(pattern = ".stow-download-", tmpdir = directory)
  on.exit(.stow_unlink(temp), add = TRUE)

  tryCatch(
    {
      .stow_download_file(url, temp, quiet)
      if (!.stow_is_regular_file(temp)) {
        stop("the download did not create a regular file")
      }
    },
    error = function(error) {
      stop(
        "Download failed.\n",
        "URL: ",
        url,
        "\n",
        "Expected file path: ",
        destination,
        "\n",
        "Original error: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  validation <- .stow_validate_file(temp, validate)
  if (!validation$valid) {
    stop(
      "Downloaded content failed validation and was not committed.\n",
      "URL: ",
      url,
      "\n",
      "Expected file path: ",
      destination,
      "\n",
      "Validation result: ",
      validation$reason,
      call. = FALSE
    )
  }

  if (replace_existing) {
    .stow_commit_replacement(temp, destination, url)
  } else {
    committed <- .stow_commit_new(temp, destination, url)
    if (committed$raced) {
      if (!.stow_is_regular_file(destination)) {
        stop(
          "Another process occupied the expected path with a non-file:\n",
          destination,
          call. = FALSE
        )
      }
      race_validation <- .stow_validate_file(destination, validate)
      if (!race_validation$valid) {
        stop(
          "A concurrently created cache file failed validation and was left unchanged.\n",
          "Expected file path: ",
          destination,
          "\n",
          "Validation result: ",
          race_validation$reason,
          call. = FALSE
        )
      }
      .stow_cache_message(destination, quiet)
    }
  }

  normalizePath(destination, winslash = "/", mustWork = TRUE)
}

.stow_check_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(value)
}

.stow_validate_file <- function(path, validate) {
  if (is.null(validate)) {
    return(list(valid = TRUE, reason = NULL))
  }
  result <- tryCatch(validate(path), error = function(error) error)
  if (inherits(result, "error")) {
    return(list(
      valid = FALSE,
      reason = paste0("validator error: ", conditionMessage(result))
    ))
  }
  if (!identical(result, TRUE)) {
    return(list(
      valid = FALSE,
      reason = "the validator did not return exactly TRUE"
    ))
  }
  list(valid = TRUE, reason = NULL)
}

.stow_offline <- function(url, exact, quiet, validate) {
  candidate <- exact
  if (!file.exists(candidate)) {
    candidate <- .stow_newest_variant(exact)
  }
  if (length(candidate) != 1L || is.na(candidate) || !file.exists(candidate)) {
    stop(
      "No cached file is available in offline mode.\n",
      "URL: ",
      url,
      "\n",
      "Expected non-ETag path: ",
      exact,
      call. = FALSE
    )
  }
  if (!.stow_is_regular_file(candidate)) {
    stop(
      "The offline cache path is not a regular file:\n",
      candidate,
      call. = FALSE
    )
  }

  validation <- .stow_validate_file(candidate, validate)
  if (!validation$valid) {
    stop(
      "Cached content failed validation in offline mode and was left unchanged.\n",
      "Cached file: ",
      candidate,
      "\n",
      "Validation result: ",
      validation$reason,
      call. = FALSE
    )
  }
  .stow_cache_message(candidate, quiet)
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

.stow_cache_message <- function(path, quiet) {
  if (!quiet) {
    message("Using cached file: ", path)
  }
  invisible(NULL)
}
