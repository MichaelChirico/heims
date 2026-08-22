context("Decode heims")

test_that("Given a raw load data file, correctly decoded", {
  skip_if_not(file.exists("~/Students/heims-tests/load_2015_sample50.csv"))
  raw_load <-
    fread("~/Students/heims-tests/load_2015_sample50.csv", na.strings = "ZZZZZZZZZZ")

  # renamed_dt <- copy(raw_dt) %>% rename_heims

  decode_heims(copy(raw_load))
})

test_that("Given a raw completions data file, correctly decoded", {
  skip_if_not(file.exists("~/Students/heims-tests/completion_2015_sample50.csv"))
  raw_enrol <-
    fread_heims("~/Students/heims-tests/completion_2015_sample50.csv")

  decode_heims(copy(raw_enrol))

})

test_that("Given a raw enrol data file, correctly decoded", {
  skip_if_not(file.exists("~/Students/heims-tests/enrol_2015_sample50.csv"))
  raw_enrol <-
    fread_heims("~/Students/heims-tests/enrol_2015_sample50.csv")

  decode_heims(copy(raw_enrol))

})

test_that("Given the enrol 2015 file, won't error", {
  file <- "~/Students/enrol2015/enrol2015.csv"
  skip_if_not(file.exists(file))
  raw_enrol2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**", "ZZZZZZZZZZ"))

  decode_heims(copy(raw_enrol2015))
})

test_that("Given the completions 2015 file, won't error", {
  file <- "~/Students/completions2015/completions2015.csv"
  skip_if_not(file.exists(file))
  raw_completions2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**", "ZZZZZZZZZZ"))

  decode_heims(copy(raw_completions2015))
})

test_that("Given the course 2015 file, won't error", {
  file <- "~/Students/course2015/course2015.csv"
  skip_if_not(file.exists(file))
  raw_course2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**", "ZZZZZZZZZZ"))

  decode_heims(copy(raw_course2015), show_progress = TRUE)
  decode_heims(copy(raw_course2015), show_progress = TRUE, check_valid = FALSE)
})

test_that("Decodes course 2015 file, retains key", {
  file <- "~/Students/course2015/course2015.csv"
  skip_if_not(file.exists(file))

  raw_course2015_keyed <- fread_heims(file)
  setkeyv(raw_course2015_keyed, c("E306", "E307"))
  course2015_keyed_decoded <- decode_heims(raw_course2015_keyed, check_valid = FALSE)
  expect_equal(key(course2015_keyed_decoded), c("HE_Provider_name", "Course_cd"))
})

test_that("Given the load 2015 file, won't error", {
  file <- "~/Students/load2015/load2015.csv"
  skip_if_not(file.exists(file))
  raw_load2015 <- fread(file, na.strings = c("", "NA", "?", ".", "*", "**", "ZZZZZZZZZZ"), colClasses = list(character = c("E313")))

  decode_heims(copy(raw_load2015))
})

test_that("Known row present and decoded correctly", {
  skip_if_not(file.exists("~/Students/2014/enrol2014.csv"))
  enrol2014 <- fread_heims("~/Students/2014/enrol2014.csv")[9456]
  enrol2014_decoded <- decode_heims(copy(enrol2014))
  expect_equal(enrol2014_decoded[["Country_of_birth"]], "Australia")
  # http://heimshelp.education.gov.au/sites/heimshelp/2012_data_requirements/2012dataelements/pages/573
  expect_equal(enrol2014_decoded[["Education_parent1"]], "Not Year 12")
  expect_equal(enrol2014_decoded[["Education_parent2"]], "Year 10")
  expect_equal(enrol2014_decoded[["Attendance_type"]], "Full-time")
  expect_equal(as.character(enrol2014_decoded[["Course_type"]]), "Bachelors Pass")
  expect_false(enrol2014_decoded[["any_disability"]])
})

test_that("Known load decoded correctly", {
  skip_if_not(file.exists("~/Students/2012/load2012.csv"))
  load_2012 <- fread_heims("~/Students/2012/load2012.csv")
  load_2012_9456 <- load_2012[9456]
  load_2012_9456_decoded <- decode_heims(copy(load_2012_9456))
  expect_equal(load_2012_9456_decoded[["Student_status_abbrev"]], "Commonwealth Supported student")
  expect_equal(load_2012_9456_decoded[["Max_student_contr"]], "Max Cth contrib.")
  expect_equal(load_2012_9456_decoded[["Special_course"]], NA_character_)
  expect_equal(load_2012_9456_decoded[["Attendance_type"]], "Full-time")
  expect_equal(load_2012_9456_decoded[["Discipline"]], "Economics")
  expect_equal(load_2012_9456_decoded[["FOE_name"]], "Business Management")
})

test_that("#17: Country of birth", {
  noE346_helper <- data.table(E347 = c("0001", "1999", "A998"))
  noE346_helper_decoded <- decode_heims(noE346_helper)
  expect_identical(noE346_helper_decoded[["Year_arrived_Aust"]], c(NA_integer_, 1999L, NA_integer_))
  expect_identical(noE346_helper_decoded[["Born_in_Aust"]], c(TRUE, NA, NA))

  wE346_helper <- data.table(E346 = as.integer(c(1101, 9232, 9231, 1101)),
                             E347 = c("0001", "1999", "A998", "A999"))

  wE346_helper_decoded <- decode_heims(wE346_helper)

  # Now the other way around
  wE346_helper_rev <- data.table(E347 = c("0001", "1999", "A998", "A999"),
                                 E346 = as.integer(c(1101, 9232, 9231, 1101)))

  wE346_helper_decoded_rev <- decode_heims(wE346_helper_rev)[]

  expect_identical(wE346_helper_rev[["Year_arrived_Aust"]], c(NA_integer_, 1999L, NA_integer_, NA_integer_))
  expect_identical(wE346_helper_rev[["Born_in_Aust"]], c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(wE346_helper_decoded_rev[["Year_arrived_Aust"]], c(NA_integer_, 1999L, NA_integer_, NA_integer_))
  expect_identical(wE346_helper_decoded_rev[["Born_in_Aust"]], c(TRUE, FALSE, FALSE, TRUE))

})

test_that("#19: Languages", {
  E348_helper <- data.table(E348 = as.integer(c(1201, 1300)))
  E348_helper_decoded <- decode_heims(E348_helper)
  expect_equal(E348_helper_decoded[["Language_home_group"]],
               c("English", "N_Europe"))
})



test_that("dummy_enrol decodes and relevels end to end", {
  # Nothing else in the suite exercises decode_heims() on data that ships with
  # the package, so validator bugs went unnoticed while every test above was
  # skipped for want of a local file.
  decoded <- decode_heims(copy(dummy_enrol))
  expect_true(is.data.table(decoded))
  expect_equal(nrow(decoded), nrow(dummy_enrol))
  expect_true("Gender" %in% names(decoded))

  # relevel_heims() must tolerate a reference level that this 5-row extract
  # simply does not contain (Language_home_group has no "English").
  releveled <- relevel_heims(decoded)
  expect_true(is.factor(releveled$Country_of_birth))
  expect_equal(levels(releveled$Country_of_birth)[1], "Australia")
})

test_that("integer64 columns keep their class through decoding", {
  # bit64 must be loaded (not merely Suggested by something else) or `[`
  # dispatches to the default method and returns a bare double.
  expect_true(is.integer64(dummy_enrol$E488))
  expect_true(is.integer64(dummy_enrol$E488[c(TRUE, FALSE, TRUE, FALSE, TRUE)]))
})
