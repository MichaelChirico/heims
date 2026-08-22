This is a package update:

* Package data.table has been moved from Depends to Imports.

* The callbacks stored in `heims_data_dict` are now bound to the package
  namespace when the dictionary is generated, so they resolve `setnames()`,
  `between()` and friends whether or not data.table is attached.

* Three element validators (E459, E487, E534) applied `&&` to a vector, which
  has been an error since R 4.3.0 and stopped `decode_heims()` from
  completing. They now reduce with `all()`.

* bit64 is now imported in NAMESPACE rather than only listed in Imports, so
  its S3 methods are registered on load and integer64 columns keep their class
  when subset.

* `relevel_heims()` no longer errors when a variable's intended reference
  level does not occur in the data supplied.

## Test environments
* local Pop!_OS 24.04 (Ubuntu 24.04) install, R 4.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes
