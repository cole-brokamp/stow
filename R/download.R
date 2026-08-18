.stow_download_file <- function(url, destfile, quiet) {
  curl::curl_download(
    url = url,
    destfile = destfile,
    quiet = quiet,
    mode = "wb"
  )
}

.stow_file_link <- function(from, to) {
  file.link(from, to)
}

.stow_file_rename <- function(from, to) {
  file.rename(from, to)
}

.stow_unlink <- function(path) {
  unlink(path, force = TRUE)
}

.stow_is_regular_file <- function(path) {
  file.exists(path) && !dir.exists(path)
}

.stow_commit_new <- function(temp, destination, url) {
  if (file.exists(destination)) {
    return(list(path = destination, raced = TRUE))
  }

  linked <- suppressWarnings(.stow_file_link(temp, destination))
  if (isTRUE(linked)) {
    .stow_unlink(temp)
    return(list(path = destination, raced = FALSE))
  }
  if (file.exists(destination)) {
    return(list(path = destination, raced = TRUE))
  }

  stop(
    "Could not atomically commit the downloaded file.\n",
    "URL: ", url, "\n",
    "Expected file path: ", destination,
    call. = FALSE
  )
}

.stow_commit_replacement <- function(temp, destination, url) {
  backup <- NA_character_
  if (file.exists(destination)) {
    if (dir.exists(destination)) {
      stop(
        "The expected file path is occupied by a directory:\n",
        destination,
        call. = FALSE
      )
    }
    backup <- tempfile(
      pattern = paste0(".", basename(destination), ".backup-"),
      tmpdir = dirname(destination)
    )
    moved <- .stow_file_rename(destination, backup)
    if (!isTRUE(moved)) {
      stop(
        "Could not preserve the existing destination before replacement.\n",
        "Existing file: ", destination, "\n",
        "Backup path: ", backup,
        call. = FALSE
      )
    }
  }

  committed <- .stow_file_rename(temp, destination)
  if (!isTRUE(committed)) {
    restored <- TRUE
    if (!is.na(backup) && file.exists(backup)) {
      if (file.exists(destination)) {
        .stow_unlink(destination)
      }
      restored <- .stow_file_rename(backup, destination)
    }
    if (!isTRUE(restored)) {
      stop(
        "Replacement failed and the existing file could not be restored.\n",
        "URL: ", url, "\n",
        "Expected file path: ", destination, "\n",
        "Recovery backup: ", backup,
        call. = FALSE
      )
    }
    stop(
      "Replacement failed; the existing destination was restored.\n",
      "URL: ", url, "\n",
      "Expected file path: ", destination,
      call. = FALSE
    )
  }

  if (!is.na(backup) && file.exists(backup)) {
    removed <- .stow_unlink(backup)
    if (!identical(removed, 0L) && file.exists(backup)) {
      warning(
        "The replacement succeeded, but its backup could not be removed: ",
        backup,
        call. = FALSE
      )
    }
  }
  destination
}
