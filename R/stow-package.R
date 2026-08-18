#' stow: Durable File Downloads and Caching
#'
#' `stow` turns a remote file URL into a durable local path. It downloads the
#' file when needed and reuses a matching cached copy on later calls, including
#' across R sessions. Cached files live in a platform-appropriate user data
#' directory, so they do not depend on the current working directory or a
#' package installation.
#'
#' The default `"stow"` cache namespace works for direct use. Package and
#' project authors can supply their own namespace and optional subdirectories
#' to keep unrelated files separate.
#'
#' @section Main functions:
#' * [stow()] downloads a file or returns a matching cached copy.
#' * [stow_path()] locates or creates a cache directory.
#' * [stow_info()] lists the files in a cache directory.
#' * [stow_prune()] removes outdated variants and orphaned internal files while
#'   retaining current usable entries.
#' * [stow_remove()] explicitly evicts all entries for one URL.
#'
#' @section Versions, validation, and durability:
#' ETags can distinguish versions served from the same URL, and offline mode
#' can reuse a previously cached copy without a network request from `stow()`.
#' Earlier variants remain available until they are explicitly managed with
#' [stow_prune()] or [stow_remove()].
#' Downloads are staged before they enter the cache. An optional validator can
#' reject both cached and newly downloaded content; a failed download or a new
#' file that fails validation is never committed as a cache entry.
#'
#' @keywords internal
"_PACKAGE"
