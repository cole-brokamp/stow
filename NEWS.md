# stow 0.1.0

* Allowed underscores and hyphens in package/project cache namespace names.
* Switched URL-directory and ETag cache keys from 32-bit to 64-bit xxHash
  values to make accidental filename collisions negligible.
* Defaulted the `package` argument of `stow()`, `stow_path()`, and
  `stow_info()` to `"stow"` for direct use while retaining separate cache
  namespaces for packages and projects.
* Added `stow()` for ETag-aware downloads, offline cache lookup, optional
  validation, and staged cache updates.
* Added `stow_path()` and `stow_info()` for locating and inspecting durable
  cache directories in platform-appropriate user data locations.
