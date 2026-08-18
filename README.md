# stow

<!-- badges: start -->
[![R-CMD-check](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`stow` turns a remote file URL into a durable local path. It downloads the
file when needed and reuses a matching cached copy on later calls, including
across R sessions. Cached files live in a platform-appropriate user data
directory, so they do not depend on the current working directory or a package
installation.

## Installation

```r
# install.packages("pak")
pak::pak("cole-brokamp/stow")
```

## Quick start

```r
library(stow)

url <- "https://github.com/geomarker-io/addr/releases/download/v1.3.0/addr-taf-v1-2025.json"
path <- stow(url)

readLines(path, n = 3)
```

`path` is the absolute path to the local copy. `stow()` stores the file but
does not read or interpret it, so the path can be passed to whichever reader
is appropriate for the file format.

## Why use stow?

- Cached files persist across R sessions and do not need to be downloaded
  again while a matching copy is available.
- The cache follows the operating system's conventions for user data instead
  of writing into a project or an installed package.
- A shared default works for direct use, while separate cache namespaces keep
  files for different packages or projects isolated.
- Cached files can be used without a network connection.
- Downloads are staged so failed transfers—and files that fail a supplied
  validator—do not become cache entries.

## Organizing and inspecting the cache

By default, files use the `"stow"` cache namespace under
`tools::R_user_dir("stow", "data")`. Locate that directory or list the files
below it with:

```r
stow_path()
stow_info()
```

Pass the same `package` and optional `subdir` values to `stow()`,
`stow_path()`, and `stow_info()` to use a separate namespace and organize
files within it:

```r
stow_path(package = "my-project", subdir = "inputs")
```

Despite the argument name, `package` may identify a package, a project, or
another cache owner. Names may contain dots, underscores, and hyphens. See
`?stow_path` for the complete naming rules and for details about relocating
the cache with `R_USER_DATA_DIR`, `XDG_DATA_HOME`, or `Sys.setenv()`.

## Versions and offline use

By default, `stow()` asks the server for an ETag, a server-provided identifier
for a particular version of a file. If the ETag changes, the new version gets
a different cache path and earlier versions are retained. If the server does
not provide an ETag or the metadata request fails, the download continues
using a URL-derived path.

Cache filenames retain the source filename and use 64-bit xxHash values to
distinguish URL directories and ETags. This lets URLs with the same basename
coexist without exposing long or unsuitable URL text in local filenames.

Use `offline = TRUE` to make `stow()` skip all network requests and require an
existing cached copy:

```r
path <- stow(url, offline = TRUE)
```

Use `overwrite = TRUE` to download again and replace the cache entry matching
the current URL and ETag. Other ETag versions are left unchanged.

## Validation and durable updates

When `validate` is supplied, it must accept one candidate file path and return
exactly `TRUE`. It is called before an existing cache entry is reused and after
new content is downloaded but before that content is committed. New downloads
are validated at a temporary path, so validators should inspect file contents
rather than rely on the filename or extension. Without a validator, `stow()`
checks that the download produced a regular file but does not infer its format
or integrity.

Invalid online cache entries trigger a replacement download. Invalid offline
entries produce an error and are left unchanged.

Cache updates are staged in the destination directory. Failed or incomplete
downloads are cleaned up and never become cache entries, and newly downloaded
content that fails validation is never committed. When replacing an entry,
`stow()` keeps the existing file until its replacement has downloaded and
validated successfully. See `?stow` for the complete cache-selection,
validation, and replacement rules.

## License

MIT
