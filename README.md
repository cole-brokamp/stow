# stow

<!-- badges: start -->
[![R-CMD-check](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`stow` is an R package that downloads files into durable, package-scoped user data directories using names derived from source URLs and entity tags. It supports offline cache lookup, content validation, and transactional replacement.

## Installation

```r
# install.packages("pak")
pak::pak("cole-brokamp/stow")
```

## Use

```r
library(stow)

path <- stow(
  "https://github.com/geomarker-io/addr/releases/download/v1.3.0/addr-taf-v1-2025.json"
)
```

By default, files live under `tools::R_user_dir("stow", "data")` and can be
located or inspected without any package configuration:

```r
stow_path()
stow_info()
```

Package authors can pass their package name to `stow()`, `stow_path()`, and
`stow_info()` to keep their files in a separate package-scoped directory.
Set `R_USER_DATA_DIR`, or another standard variable honored by
`tools::R_user_dir()`, to relocate package data.

Cache names use 64-bit xxHash values for URL directories and, when available,
ETags. If metadata probing is not supported, the download still proceeds with
a URL-derived name. Cached ETag variants are retained rather than deleted
automatically.

Use `offline = TRUE` to prohibit network operations. Offline lookup checks the
non-ETag path first, then selects the newest ETag variant with lexical ordering
as a deterministic timestamp tie-breaker.

## Validation and durability

When `validate` is supplied, it must accept one candidate file path and return
exactly `TRUE`. It is called before an existing cache entry is reused and after
new content is downloaded but before that content is committed. New downloads
are validated at a temporary path, so validators should inspect file contents
rather than rely on the filename or extension. Without a validator, `stow()`
checks that the download produced a regular file but does not infer its format
or integrity.

Invalid online cache entries trigger a replacement download. Invalid offline
entries produce an error and are left unchanged.

Durable cache updates are staged in the destination directory. Failed or
incomplete downloads are cleaned up and never become cache entries, and newly
downloaded content that fails validation is never committed. When replacing a
cache entry, the existing file is preserved until the replacement has
downloaded and validated successfully; if the commit fails, `stow()` attempts
to restore the existing file. An existing invalid entry may remain on disk if
its replacement fails, but it is not returned as valid.
