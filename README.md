# stow

<!-- badges: start -->
[![R-CMD-check](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cole-brokamp/stow/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/stow)](https://CRAN.R-project.org/package=stow)
<!-- badges: end -->

`stow` turns a remote file URL into a durable managed local copy. It downloads
the file when needed and reuses a matching copy on later calls, including
across R sessions and without a network connection. Managed local copies live
in a fixed `stow` subdirectory beneath a platform-appropriate,
package-specific user data directory, so they do not depend on the current
working directory or a package installation.

## Installation

```r
install.packages("stow")
```

## Quick start

```r
library(stow)

url <- "https://github.com/geomarker-io/addr/releases/download/v1.3.0/addr-taf-v1-2025.json"
path <- stow(url)

readLines(path, n = 3)
```

`path` is the absolute path to the managed local copy. `stow()` stores the file
but does not read or interpret it, so the path can be passed to whichever
reader is appropriate for the file format.

## Why use stow?

- Managed local copies persist across R sessions and do not need to be
  downloaded again while a matching copy is available.
- The managed local copy directory follows the operating system's conventions
  for durable user data instead of writing into a working directory or an
  installed package.
- A shared default works for direct use, while each consuming package can own
  its managed local copies in its package-specific user data directory.
- Managed local copies can be used without a network connection.
- Downloads are staged so failed transfers—and files that fail a supplied
  validator—do not become managed local copies.
- Retained files can be inspected, conservatively pruned, or explicitly
  removed through managed-local-copy lifecycle functions.

## Organizing and managing local copies

For direct use, files are stored beneath:

```r
file.path(tools::R_user_dir("stow", "data"), "stow")
```

Locate that directory or list the managed local copies below it with:

```r
stow_path()
stow_info()
```

A package that uses `stow` should pass its own package name to `stow()` and the
related lifecycle functions. The package name selects its package-specific
user data directory; `stow` then uses a fixed `stow` subdirectory beneath it.
An optional `subdir` organizes managed local copies within that directory:

```r
stow_path(package = "myPackage", subdir = "inputs")
```

`package` must be a syntactically valid R package name: at least two
characters, beginning with an ASCII letter, containing only ASCII letters,
numbers, and periods, and ending with a letter or number. See `?stow_path` for
details about relocating package-specific user data with `R_USER_DATA_DIR`,
`XDG_DATA_HOME`, or `Sys.setenv()`.

`stow_prune()` provides conservative lifecycle management. With no `url`, it
removes only aged, orphaned stow temporary files and replacement backups. With
a `url`, it can also remove aged ETag variants while always retaining the
non-ETag copy and the newest ETag fallback. Files must be at least 30 days old
by default. Preview the exact files first with `dry_run = TRUE`:

```r
stow_prune(dry_run = TRUE)
stow_prune(url, keep = path, dry_run = TRUE)
```

Passing the path returned by a recent `stow()` call through `keep` protects it
in addition to the automatic retention rules. This is useful if a server has
returned to an older ETag version. Cleanup makes no network requests, does not
remove directories or unrecognized files, and stays within the selected
`package` and `subdir`.

When the intent is to remove the current managed local copy as well as every
ETag variant for one known URL, use the explicit URL-scoped removal function:

```r
stow_remove(url, dry_run = TRUE)
stow_remove(url)
```

`stow_remove()` leaves copies for other URLs and internal recovery artifacts
unchanged. A consuming package must pass the same `package` and `subdir` used
for the original download.

## Versions and offline use

By default, `stow()` asks the server for an ETag, a server-provided identifier
for a particular version of a file. If the ETag changes, the new version gets
a different managed local copy path and earlier versions are retained until
they are explicitly managed with `stow_prune()` or `stow_remove()`. If the
server does not provide an ETag or the metadata request fails, the download
continues using a URL-derived path.

Managed local copy filenames retain the source filename and use 64-bit xxHash
values to distinguish URL directories and ETags. This lets URLs with the same
basename coexist without exposing long or unsuitable URL text in local
filenames.

Use `offline = TRUE` to make `stow()` skip all network requests and require an
existing managed local copy:

```r
path <- stow(url, offline = TRUE)
```

Use `overwrite = TRUE` to download again and replace the managed local copy
matching the current URL and ETag. Other ETag versions are left unchanged.

## Validation and durable updates

When `validate` is supplied, it must accept one candidate file path and return
exactly `TRUE`. It is called before an existing managed local copy is reused
and after new content is downloaded but before that content is committed. New
downloads are validated at a temporary path, so validators should inspect file
contents rather than rely on the filename or extension. Without a validator,
`stow()` checks that the download produced a regular file but does not infer
its format or integrity.

Invalid online managed local copies trigger a replacement download. Invalid
offline copies produce an error and are left unchanged.

Updates are staged in the destination directory. Failed or incomplete
downloads are cleaned up and never become managed local copies, and newly
downloaded content that fails validation is never committed. When replacing a
copy, `stow()` keeps the existing file until its replacement has downloaded
and validated successfully. See `?stow` for the complete selection,
validation, and replacement rules.
