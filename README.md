# stow

<!-- badges: start -->
[![R-CMD-check](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`stow` provides durable file downloading and caching. It does not read,
transform, publish, authenticate, or manage artifacts.

## Installation

```r
# install.packages("pak")
pak::pak("cole-brokamp/stow")
```

## Use

```r
library(stow)

path <- stow(
  "https://github.com/geomarker-io/addr/releases/download/v1.3.0/addr-taf-v1-2025.json",
  validate = function(path) {
    any(
      grepl(
        '"artifact_type": "addr-taf-fuel"',
        readLines(path),
        fixed = TRUE
      )
    )
  }
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

When `validate` is supplied, it must accept one path and return exactly `TRUE`.
Invalid online cache entries are refreshed; invalid offline entries are left in
place and reported as errors. Downloads are validated before transactional
commit, so a failed replacement does not destroy the existing destination.

## Scope

`stow` intentionally has no APIs for removal, reading, transformation, GitHub
releases, authentication, checksums, manifests, or resumable transfers.

## License

MIT
