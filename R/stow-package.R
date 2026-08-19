#' stow: Durable Managed Local Copies of Remote Files
#'
#' `stow` turns a remote file URL into a durable managed local copy. It
#' downloads the file when needed and reuses a matching copy on later calls,
#' including across R sessions. Managed local copies live in a fixed `stow`
#' subdirectory beneath a platform-appropriate, package-specific user data
#' directory, so they do not depend on the current working directory or a
#' package installation.
#'
#' The default package name, `"stow"`, works for direct use. A package that uses
#' `stow` supplies its own package name so that its managed local copies remain
#' separate from those owned by other packages.
#'
#' @section Main functions:
#' * [stow()] downloads a file or returns a matching managed local copy.
#' * [stow_path()] locates or creates the managed local copy directory.
#' * [stow_info()] lists managed local copies.
#' * [stow_prune()] removes outdated variants and orphaned internal files while
#'   retaining current usable entries.
#' * [stow_remove()] explicitly removes all managed local copies for one URL.
#'
#' @section Versions, validation, and durability:
#' ETags can distinguish versions served from the same URL, and offline mode
#' can reuse a managed local copy without a network request from `stow()`.
#' Earlier variants remain available until they are explicitly managed with
#' [stow_prune()] or [stow_remove()].
#' Downloads are staged before they become managed local copies. An optional
#' validator can reject both existing and newly downloaded content; a failed
#' download or a new file that fails validation is never committed.
#'
#' @keywords internal
"_PACKAGE"
