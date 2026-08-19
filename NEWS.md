# stow 0.2.1

* Restricted `package` to syntactically valid R package names. It now names
  only the package that owns the managed local copies; underscores, hyphens,
  and other identifiers are rejected.
* Added a fixed `stow` subdirectory beneath each package-specific user data
  directory. Existing copies stored directly in the package directory are not
  discovered at the new location and may need to be downloaded again.
* Replaced prior storage terminology with managed local copy terminology to
  reflect that files are stored as durable package data for reliable offline
  use.

# stow 0.2.0

* Prevented pruning and removal from following symlinked directories or
  resolving managed-local-copy symlinks to targets outside the selected
  directory.
* Added `stow_prune()` for package-aware removal of aged, superseded ETag
  variants and orphaned stow temporary/backup files. Conservative defaults
  retain the non-ETag copy, the newest ETag fallback, recent files, and
  recovery backups whose destination is missing; dry runs and explicit
  additional protections are supported.
* Added `stow_remove()` for explicit, URL-scoped removal of the current managed
  local copy and all of its ETag variants without removing unrelated files.

# stow 0.1.0

* Initially accepted identifiers broader than valid R package names.
* Switched URL-directory and ETag filename keys from 32-bit to 64-bit xxHash
  values to make accidental filename collisions negligible.
* Defaulted the `package` argument of `stow()`, `stow_path()`, and
  `stow_info()` to `"stow"` for direct use while retaining separate package
  directories.
* Added `stow()` for ETag-aware downloads, offline lookup, optional validation,
  and staged updates.
* Added `stow_path()` and `stow_info()` for locating and inspecting durable
  local copies in platform-appropriate user data locations.
