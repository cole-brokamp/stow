# stow 0.0.0.9000

* Switched URL-directory and ETag cache keys from 32-bit to 64-bit xxHash
  values to make accidental filename collisions negligible.
* Defaulted the `package` argument of `stow()`, `stow_path()`, and
  `stow_info()` to `"stow"` for direct use while retaining package-scoped
  caches for package authors.
* Added `stow()` for ETag-aware, validated, transactional package-scoped
  downloads and offline cache lookup.
* Added `stow_path()` and `stow_info()` for locating and inspecting durable
  package data directories.
