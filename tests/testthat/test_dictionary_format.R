context("Dictionary format")

test_that("No repeated long_names", {
  dict_names <- browse_elements(".")
  expect_false(any(duplicated(dict_names$long_name)))
  expect_false(any(duplicated(dict_names$orig_name)))
})



test_that("Dictionary callbacks are bound to the heims namespace", {
  # The callbacks are closures serialized in data/heims_data_dict.rda. If they
  # are bound to .GlobalEnv they can only find setnames(), between(), %fin%
  # etc. when data.table happens to be attached, which heims no longer
  # guarantees (data.table is Imports, not Depends). data-raw rebinds them.
  ns <- asNamespace("heims")
  for (element in names(heims_data_dict)) {
    entry <- heims_data_dict[[element]]
    for (callback in names(entry)[vapply(entry, is.function, logical(1))]) {
      expect_identical(environment(entry[[callback]]), ns,
                       info = paste0(element, "$", callback))
    }
  }
})

test_that("Dictionary callbacks resolve without data.table attached", {
  # Every function the callbacks call must be reachable from the namespace
  # *without* falling through to the search path (a namespace's parents end at
  # base, whose parent is .GlobalEnv, so plain exists() would be satisfied by a
  # merely-attached data.table). Bare data.table column names are excluded:
  # they are evaluated by [.data.table in the frame of the table, not here.
  visible_to_namespace <- function(name) {
    env <- asNamespace("heims")
    while (!identical(env, globalenv())) {
      if (exists(name, envir = env, inherits = FALSE)) {
        return(TRUE)
      }
      env <- parent.env(env)
    }
    FALSE
  }

  for (element in names(heims_data_dict)) {
    entry <- heims_data_dict[[element]]
    for (callback in names(entry)[vapply(entry, is.function, logical(1))]) {
      called <- codetools::findGlobals(entry[[callback]], merge = FALSE)$functions
      unreachable <- called[!vapply(called, visible_to_namespace, logical(1))]
      expect_identical(unreachable, character(0),
                       info = paste0(element, "$", callback, ": ",
                                     toString(unreachable)))
    }
  }
})
