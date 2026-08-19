#' Download a managed local copy
#'
#' `stow()` turns a remote file URL into an absolute path to a durable managed
#' local copy. It downloads the file when a matching copy is not available and
#' otherwise reuses the existing copy, including across R sessions. The
#' directory is created and located by [stow_path()].
#'
#' @param url A single `https`, `http`, `ftp`, or `ftps` URL. Its path must end
#'   in a filename. Query strings and fragments are not supported.
#' @param package The name of the R package that owns the managed local copy.
#'   It must be a syntactically valid R package name: at least two characters,
#'   beginning with an ASCII letter, containing only ASCII letters, numbers,
#'   and periods, and ending with a letter or number. The default, `"stow"`, is
#'   intended for direct use. A package that uses `stow` should supply its own
#'   package name. Copies are stored in a fixed `stow` subdirectory beneath the
#'   package-specific user data directory returned by [tools::R_user_dir()].
#' @param subdir An optional relative subdirectory within the package's fixed
#'   `stow` directory.
#'   Each component must begin with a letter or number, may otherwise contain
#'   letters, numbers, dots, underscores, or hyphens, and must not end in a
#'   dot. Absolute paths, empty components, `.` components, and `..` components
#'   are rejected.
#' @param overwrite Whether to download again and replace the managed local
#'   copy that matches the current URL and ETag. Copies for other ETags are
#'   retained.
#' @param offline Whether `stow()` must make no network requests and use only a
#'   managed local copy. It cannot be combined with `overwrite = TRUE`.
#' @param quiet Whether to suppress informational messages and download
#'   progress. Warnings and errors are never suppressed.
#' @param etag Whether online calls should ask the server for an ETag and use it
#'   to distinguish versions. If an ETag is unavailable or the request fails,
#'   `stow()` continues with a URL-derived filename.
#' @param validate An optional content-validation function called with one
#'   candidate file path. It must return exactly `TRUE`; an error or any other
#'   result marks the file invalid.
#'
#' @section Managed local copy identity and versions:
#' Managed local copy filenames retain the URL basename and include a 64-bit
#' xxHash of the URL directory. This distinguishes URLs that have the same
#' basename. When a server-provided ETag is available, its 64-bit xxHash is
#' inserted before the file extension. A changed ETag therefore creates a new
#' managed local copy path, and earlier ETag variants are retained until
#' explicitly managed with [stow_prune()] or [stow_remove()].
#'
#' Online calls reuse an existing matching copy unless `overwrite = TRUE` or
#' the copy fails `validate`. If the ETag request fails or is unsupported,
#' downloading continues with the URL-derived, non-ETag name.
#'
#' In offline mode, `stow()` makes no network requests. It first looks for the
#' non-ETag copy and otherwise chooses the newest matching ETag variant by
#' modification time, using lexical filename order to break ties.
#'
#' @section Validation:
#' When `validate` is supplied, `stow()` calls it before reusing an existing
#' managed local copy and after downloading new content but before committing
#' that content. It is also applied to a managed local copy created by another
#' process before that copy is reused. A validator error, `FALSE`, or any value
#' other than exactly `TRUE` marks the candidate invalid.
#'
#' New downloads are validated at a temporary path, so validators should
#' inspect file contents rather than rely on the temporary filename or its
#' extension. A validator can, for example, check a file size, parse expected
#' metadata, open a serialized object, or verify a checksum.
#'
#' An invalid online managed local copy triggers a replacement download. In
#' offline mode, an invalid copy produces an error and is left unchanged. When
#' `validate = NULL`, `stow()` checks that a download produced a regular file
#' but makes no claim about its format, integrity, or meaning.
#'
#' @section Durable managed local copy updates:
#' Downloads are written to a temporary file inside the destination directory.
#' A failed or incomplete download is cleaned up and never becomes a managed
#' local copy. Newly downloaded content that fails `validate` is likewise never
#' committed to its destination filename.
#'
#' When replacing an existing copy, `stow()` keeps the existing file until the
#' replacement has downloaded and validated successfully. If committing the
#' replacement fails, it attempts to restore the existing file. Thus a failed
#' download or invalid replacement does not overwrite an existing managed
#' local copy. An existing copy that fails validation may remain on disk if its
#' replacement fails, but it is not returned as valid.
#'
#' @return The absolute path to the managed local copy as a visible character
#'   scalar. `stow()` does not read or interpret the file's contents.
#' @seealso [stow_path()] to locate managed local copies, [stow_info()] to list
#'   them, and [stow_prune()] to manage retained copies.
#' @export
#'
#' @examples
#' \donttest{
#' url <- "https://github.com/geomarker-io/addr/releases/download/v1.3.0/addr-taf-v1-2025.json"
#'
#' tryCatch(
#'   withr::with_envvar(c(R_USER_DATA_DIR = tempfile("stow-data-")), {
#'     path <- stow(url)
#'     readLines(path, n = 3)
#'
#'     is_addr_manifest <- function(path) {
#'       text <- paste(readLines(path, warn = FALSE), collapse = "\n")
#'       grepl('"artifact_type": "addr-taf-fuel"', text, fixed = TRUE)
#'     }
#'     stow(url, validate = is_addr_manifest)
#'   }),
#'   error = function(error) {
#'     message("Skipping remote example: ", conditionMessage(error))
#'   }
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
        .stow_copy_message(destination, quiet)
        return(normalizePath(destination, winslash = "/", mustWork = TRUE))
      }
      replace_existing <- TRUE
      if (!quiet) {
        message(
          "Managed local copy failed validation; downloading a replacement:\n  ",
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
          "A concurrently created managed local copy failed validation and was left unchanged.\n",
          "Expected file path: ",
          destination,
          "\n",
          "Validation result: ",
          race_validation$reason,
          call. = FALSE
        )
      }
      .stow_copy_message(destination, quiet)
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
      "No managed local copy is available in offline mode.\n",
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
      "The offline managed local copy path is not a regular file:\n",
      candidate,
      call. = FALSE
    )
  }

  validation <- .stow_validate_file(candidate, validate)
  if (!validation$valid) {
    stop(
      "Managed local copy failed validation in offline mode and was left unchanged.\n",
      "Managed local copy: ",
      candidate,
      "\n",
      "Validation result: ",
      validation$reason,
      call. = FALSE
    )
  }
  .stow_copy_message(candidate, quiet)
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

.stow_copy_message <- function(path, quiet) {
  if (!quiet) {
    message("Using managed local copy: ", path)
  }
  invisible(NULL)
}
