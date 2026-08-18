.stow_url_details <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    stop("`url` must be one non-missing character string.", call. = FALSE)
  }
  if (grepl("[?#]", url)) {
    stop(
      "URLs with query strings or fragments are not supported.",
      call. = FALSE
    )
  }
  if (grepl("[[:space:][:cntrl:]]", url) || grepl("\\\\", url)) {
    stop("`url` contains unsupported characters.", call. = FALSE)
  }

  match <- regexec("^([A-Za-z][A-Za-z0-9+.-]*)://", url)
  pieces <- regmatches(url, match)[[1L]]
  if (length(pieces) == 0L) {
    stop("`url` must be an absolute URL.", call. = FALSE)
  }
  scheme <- tolower(pieces[[2L]])
  if (!scheme %in% c("https", "http", "ftp", "ftps")) {
    stop(
      "`url` must use the https, http, ftp, or ftps scheme.",
      call. = FALSE
    )
  }

  remainder <- substring(url, nchar(pieces[[1L]]) + 1L)
  first_slash <- regexpr("/", remainder, fixed = TRUE)[[1L]]
  if (first_slash < 1L) {
    stop("`url` must include a filename.", call. = FALSE)
  }
  authority <- substr(remainder, 1L, first_slash - 1L)
  path <- substring(remainder, first_slash)
  if (!nzchar(authority) || grepl("@", authority, fixed = TRUE)) {
    stop(
      "`url` must have a host and must not contain embedded credentials.",
      call. = FALSE
    )
  }
  if (!nzchar(path) || endsWith(path, "/")) {
    stop("`url` must include a filename.", call. = FALSE)
  }

  filename <- basename(path)
  if (!nzchar(filename) || filename %in% c(".", "..")) {
    stop("`url` must include a filename.", call. = FALSE)
  }
  if (grepl("[<>:\"|*]", filename) || endsWith(filename, ".")) {
    stop(
      "The URL filename is not safe on supported file systems.",
      call. = FALSE
    )
  }

  list(
    url = url,
    directory = dirname(url),
    filename = filename
  )
}

.stow_url_to_filename <- function(url, etag = NULL) {
  details <- .stow_url_details(url)
  directory_hash <- digest::digest(
    details$directory,
    algo = "xxhash64",
    serialize = FALSE
  )
  filename <- paste0(directory_hash, "--", details$filename)

  if (is.null(etag) || (length(etag) == 1L && is.na(etag))) {
    return(filename)
  }
  etag_hash <- .stow_etag_hash(etag)
  if (is.na(etag_hash)) {
    return(filename)
  }
  .stow_insert_etag_hash(filename, etag_hash)
}

.stow_insert_etag_hash <- function(filename, etag_hash) {
  extension <- tools::file_ext(filename)
  if (!nzchar(extension)) {
    return(paste0(filename, "--", etag_hash))
  }
  paste0(
    tools::file_path_sans_ext(filename),
    "--",
    etag_hash,
    ".",
    extension
  )
}

.stow_normalize_etag <- function(etag) {
  if (!is.character(etag) || length(etag) != 1L || is.na(etag)) {
    return(NA_character_)
  }
  etag <- trimws(etag)
  etag <- sub("^[Ww]/[[:space:]]*", "", etag)
  etag <- trimws(etag)
  if (
    nchar(etag) >= 2L && startsWith(etag, "\"") && endsWith(etag, "\"")
  ) {
    etag <- substr(etag, 2L, nchar(etag) - 1L)
  }
  etag <- gsub("\\\\\"", "\"", etag, fixed = TRUE)
  if (!nzchar(etag)) {
    return(NA_character_)
  }
  etag
}

.stow_etag_hash <- function(etag) {
  etag <- .stow_normalize_etag(etag)
  if (is.na(etag)) {
    return(NA_character_)
  }
  digest::digest(etag, algo = "xxhash64", serialize = FALSE)
}

.stow_probe_etag <- function(url) {
  response <- curl::curl_fetch_memory(
    url,
    handle = curl::new_handle(
      nobody = TRUE,
      header = TRUE,
      followlocation = TRUE,
      failonerror = FALSE
    )
  )
  if (is.null(response$status_code) || response$status_code >= 400L) {
    return(NA_character_)
  }

  headers <- curl::parse_headers(response$headers)
  positions <- grep("^ETag[[:space:]]*:", headers, ignore.case = TRUE)
  if (length(positions) == 0L) {
    return(NA_character_)
  }
  sub(
    "^ETag[[:space:]]*:[[:space:]]*",
    "",
    headers[[positions[[length(positions)]]]],
    ignore.case = TRUE
  )
}

.stow_variant_candidates <- function(exact_path) {
  directory <- dirname(exact_path)
  if (!dir.exists(directory)) {
    return(character())
  }
  exact_name <- basename(exact_path)
  extension <- tools::file_ext(exact_name)
  if (nzchar(extension)) {
    prefix <- paste0(tools::file_path_sans_ext(exact_name), "--")
    suffix <- paste0(".", extension)
  } else {
    prefix <- paste0(exact_name, "--")
    suffix <- ""
  }

  candidates <- list.files(directory, full.names = TRUE, all.files = TRUE)
  names <- basename(candidates)
  possible <- startsWith(names, prefix)
  if (nzchar(suffix)) {
    possible <- possible & endsWith(names, suffix)
  }
  candidates <- candidates[possible]
  names <- names[possible]
  if (length(candidates) == 0L) {
    return(character())
  }

  token_end <- nchar(names) - nchar(suffix)
  tokens <- substring(names, nchar(prefix) + 1L, token_end)
  keep <- grepl("^[0-9a-f]{16}$", tokens)
  candidates <- candidates[keep]
  if (length(candidates) == 0L) {
    return(character())
  }

  info <- file.info(candidates)
  candidates[!is.na(info$isdir) & !info$isdir & !is.na(info$mtime)]
}

.stow_newest_variant <- function(exact_path) {
  candidates <- .stow_variant_candidates(exact_path)
  if (length(candidates) == 0L) {
    return(NA_character_)
  }
  modified <- as.numeric(file.info(candidates)$mtime)
  candidates[order(-modified, basename(candidates), method = "radix")][[1L]]
}
