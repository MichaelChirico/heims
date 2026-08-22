# Keep source references: once the callbacks below are serialized into data/,
# this file is the only place their code exists in readable form, so printing
# heims_data_dict$E091$decoder should show it as written rather than deparsed.
# (Rscript defaults keep.source to FALSE.)
options(keep.source = TRUE)

library(lubridate)
library(magrittr)
library(heims)
library(fastmatch)
library(data.table)

source("./R/utils.R")

list(
  "E089" = list(long_name = "Initial_student_record_ind",
                orig_name = "E089",
                validate = function(v) is.integer(v) && all(between(v, 1, 2)),
                valid = function(v) if (is.integer(v)) between(v, 0, 1) else v %fin% c(0, 1),
                decoder = E089_decoder),

  "E091" = list(long_name = "Semester_1",
                orig_name = "E091",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 0, 1)),
                valid = function(v) if (is.integer(v)) between(v, 0, 1) else v %fin% c(0, 1),
                decoder = function(DT){
                  DT[, E091 := as.logical(E091)]
                  setnames(DT, "E091", "Semester_1")
                  DT
                }),

  "E092" = list(long_name = "Semester_2",
                orig_name = "E092",
                mark_missing = never,
                validate = function(v) !any(is.na(as.logical(v))),
                valid = function(v) !is.na(as.logical(v)),
                decoder = function(DT){
                  DT[, E092 := as.logical(E092)]
                  setnames(DT, "E092", "Semester_2")
                  DT
                }),

  "E095" = list(long_name = "Student_course_combn_is_first",
                orig_name = "E095",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 1, 2)),
                valid = function(v) if (is.integer(v)) between(v, 1, 2) else v %fin% c(1, 2),
                decoder = E095_decoder),
  "E300" = list(long_name = "Record_type_cd",
                orig_name = "E300",
                mark_missing = never,
                # validate = function(v) is.character(v) && all(v %fin% c("#", "$", "%", "1", "2", "3")),
                validate = function(v) is.integer(v) && all(between(v, 1, 3), na.rm = TRUE),
                ad_hoc_validation_note = "HEIMS dictionary says this is a character field, but the only value used in 2005-20015 was '2' so cast as integer for efficiency",
                valid = function(v) v == 2),
  "E306" = list(long_name = "HE_Provider_name",
                orig_name = "E306",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v) && all(or(v == 0L,
                                                               between(v, 1000, 9999)), na.rm = TRUE),
                valid = function(v) or(v == 0L, between(v, 1000, 9999)),
                decoder = E306_decoder),
  "E307" = list(long_name = "Course_cd",
                orig_name = "E307",
                mark_missing = never,
                validate = always,
                valid = every),
  "E308" = list(long_name = "Course_name_inclMajor",
                orig_name = "E308",
                mark_missing = never,
                validate = always,
                valid = every),
  "E310" = list(long_name = "Course_type",
                orig_name = "E310",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(v %fin% c(1, 2, 12, 14, 3:7,
                                                                      11, 8:10, 13, 20:22,
                                                                      30, 41, 42, 50, 60,
                                                                      61, 80, 81, 82, 99)),
                valid = function(v) v %fin% c(1, 2, 12, 14, 3:7,
                                              11, 8:10, 13, 20:22,
                                              30, 41, 42, 50, 60,
                                              61, 80, 81, 82, 99),
                decoder = E310_decoder),
  "E312" = list(long_name = "Special_course",
                orig_name = "E312",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(v %fin% c(0, 15, 21:23, 25:27)),
                valid = function(v) v %fin% c(0, 15, 21:23, 25:27),
                decoder = E312_decoder),
  "E313" = list(long_name = "Student_id",
                orig_name = "E313",
                ad_hoc_prepare = rm_leading_0s,
                mark_missing = never,
                validate = always,
                valid = every),
  "E314" = list(long_name = "DOB",
                orig_name = "E314",
                mark_missing = function(v) v %fin% c(19010101, 19000101, 18991230),
                validate = function(v) is.integer(v) && all(is.Date(v)),
                valid = function(v) is.Date(v),
                decoder = function(DT){
                  stopifnot("E314" %in% names(DT))
                  E314 <- NULL
                  DT[, "DOB" := ymd(E314)]
                  DT[, E314 := NULL]
                },
                post_fst = function(DT){
                  setattr(DT[["DOB"]], "class", "Date")
                }),
  "E315" = list(long_name = "Gender",
                orig_name = "E315",
                mark_missing = function(v) v %fin% c("X", "U"),
                validate = function(v) all(v %fin% c("M", "F", "X", "U")),
                ad_hoc_validation_note = "The value 'U' was also used in 14 cases. Cast as 'M'.",
                valid = function(v) v %fin% c("M", "F", "X", "U"),
                decoder = function(DT){
                  coalesce_gender <- function(g) {g[!{g %fin% c("M", "F")}] <- "M"; g}
                  DT[, Gender := coalesce_gender(E315)]
                  DT[, "E315" := NULL]
                }),

  "E316" = list(long_name = "ATSI_cd",
                orig_name = "E316",
                mark_missing = function(v) v == 9,
                validate = function(v) is.integer(v) && all(v %in% c(2:5, 9)),
                valid = function(v) v %fin% c(2:5, 9),
                decoder = E316_decoder),

  "E319" = list(long_name = "Term_location",
                orig_name = "E319",
                mark_missing = function(v) substr(v, 2, 5) == "9999",
                validate = function(v) AND(AND(is.character(v),
                                               all(nchar(v) == 5)),
                                           all(or(substr(v, 2, 5) == "9999",
                                                  or(v %fin% paste0("X", 1200:9299),
                                                     between(as.integer(substr(v, 2, 5)), 1, 9998))))),
                valid = function(v) and(nchar(v) == 5,
                                        or(substr(v, 2, 5) == "9999",
                                           or(v %fin% paste0("X", 1200:9299),
                                              between(as.integer(substr(v, 2, 5)), 1, 9998))))),

  "E320" = list(long_name = "Home_location",
                orig_name = "E320",
                mark_missing = function(v) substr(v, 2, 5) == "9999",
                validate = function(v){
                  v <- v[!is.na(v)]
                  AND(AND(is.character(v),
                          all(nchar(v) == 5)),
                      all(or(substr(v, 2, 5) == "9999",
                             or(v %fin% paste0("X", 1200:9299),
                                between(as.integer(substr(v, 2, 5)), 1, 9998)))))
                },
                valid = function(v) or(substr(v, 2, 5) == "9999",
                                       or(v %fin% paste0("X", 1200:9299),
                                          between(as.integer(substr(v, 2, 5)), 1, 9998)))),



  "E327" = list(long_name = "New_admission_basis",
                orig_name = "E327",
                mark_missing = function(v) v %fin% c(2L, 3L, 99L),
                ad_hoc_validation_note = "Values 2 and 3 were observed and preserved despite being not valid according to the dictionary.",
                validate = function(v) is.integer(v) && all(v %fin% c(1L, 2L, 3L, 31L, 33L, 34L, 36L, 37L, 29L, 99L), na.rm = TRUE),
                valid = function(v) v %fin% c(1L, 2L, 3L, 31L, 33L, 34L, 36L, 37L, 29L, 99L),
                decoder = E327_decoder),

  "E328" = list(long_name = "Course_commencement_date",
                orig_name = "E328",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(is.YearMonth(v)),
                valid = function(v) is.YearMonth(v),
                decoder = function(DT){
                  stopifnot("E328" %in% names(DT))
                  E314 <- NULL
                  DT[, "Course_commencement_date" := ymd(E328 * 100 + 1)]
                  DT[, E328 := NULL]
                },
                post_fst = function(DT){
                  setattr(DT[["Course_commencement_date"]], "class", "Date")
                }),
  "E329" = list(long_name = "Mode_of_attendance",
                orig_name = "E329",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 1, 5)),
                valid = function(v) if (is.integer(v)) between(v, 1, 5) else v %fin% c(1, 2, 3, 4, 5),
                decoder = E329_decoder),
  "E330" = list(long_name = "Attendance_type",
                orig_name = "E330",
                mark_missing = function(v) v == 9L,
                validate = function(v) is.integer(v) && all(v %fin% c(0L, 1L, 2L, 9L)),
                valid = function(v) v %fin% c(0, 1, 2, 9),
                decoder = E330_decoder),

  "E331" = list(long_name = "Maj_course_ind",
                orig_name = "E331",
                mark_missing = never,
                ad_hoc_validation_note = "Value of '4' present in 11 entries but not valid in dictionary. Left as-is.",
                validate = function(v) is.integer(v) && all(between(v, 1, 4)),
                valid = function(v) if (is.integer(v)) between(v, 1, 4) else v %fin% c(1, 2, 3),
                decoder = E331_decoder),

  "E333" = list(long_name = "Academic_org",
                orig_name = "E333",
                mark_missing = never,
                validate = always,
                valid = every),
  "E335" = list(long_name = "Academic_org_unit_grp_cd",
                orig_name = "E335",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 0, 1299)),
                valid = function(v) if (is.integer(v)) between(v, 0, 1299) else v %fin% seq.int(0, 1299)),
  "E337" = list(long_name = "Industry_work_experience",
                orig_name = "E337",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 0, 2)),
                valid = function(v) if (is.integer(v)) between(v, 0, 2) else v %fin% c(0, 1, 2),
                decoder = E337_decoder),
  "E339" = list(long_name = "EFTSL",
                orig_name = "E339",
                mark_missing = never,
                # Was originally
                # validate = function(v) is.double(v) && all(between(v, 0, 1)),
                # but due to three elements EFTSL = {1.25, 1.50, 3.00}
                # now use
                validate = function(v) is.double(v) && all(between(v, 0, 3)),
                ad_hoc_validation_note = "Three elements had EFTSL of 1.25, 1.50, and 3.00. The EFTSL = 3.00 load also had a start date of 2003, suggesting coalescing of loads into one insert. Left as-is.",
                valid = function(v) between(v, 0, 3)),

  "E346" = list(long_name = "Country_of_birth",
                orig_name = "E346",
                mark_missing = function(v) v >= 9998,
                validate = function(v) is.integer(v) && all(v %fin% E346_decoder[["E346"]], na.rm = TRUE),
                valid = function(v) v %fin% E346_decoder[["E346"]],
                decoder = E346_decoder),

  "E347" = list(long_name = "Year_arrived_Aust",
                orig_name = "E347",
                mark_missing = function(v) substr(v, 1, 1) == "A",
                validate = function(v) is.character(v) && all(v %fin% c("0000",
                                                                        "0001",
                                                                        seq.int(1900,
                                                                                2099),
                                                                        "A998",
                                                                        "A999"),
                                                              na.rm = TRUE),
                valid = function(v) v %fin% c("0000",
                                              "0001",
                                              seq.int(1900,
                                                      2099),
                                              "A998",
                                              "A999"),
                decoder = function(DT) {
                  DT[, Year_arrived_Aust := force_integer(E347)]
                  DT[, Year_arrived_Aust := if_else(E347 %fin% c("0000", "0001"),
                                                    NA_integer_,
                                                    Year_arrived_Aust)]
                  # If year of arrival does not assert the person was born in Australia,
                  # it is unknown.
                  DT[, Born_in_Aust := E347 == "0001" | NA]

                  # However, if the Country of birth is Australia, we can assert whether
                  # the person was born in Australia.
                  if (any(c("E346", "Country_of_birth") %in% names(DT))) {
                    switch(intersect(c("E346", "Country_of_birth"), names(DT)),
                           "E346" = {
                             DT[, Born_in_Aust := coalesce(Born_in_Aust, DT[["E346"]] %fin% c(1100, 1101))]
                           },
                           "Country_of_birth" = {
                             DT[, Born_in_Aust := coalesce(Born_in_Aust, DT[["Country_of_birth"]] == "Australia")]
                           })
                  }
                  DT[, E347 := NULL]
                  DT
                }),

  "E348" = list(long_name = "Language_home",
                orig_name = "E348",
                mark_missing = function(v) v == 0L | v >= 9998,
                validate = function(v) is.integer(v) && all(v %fin% c(0L, 1L,
                                                                      1201L, # English
                                                                      seq.int(1000, 1199),
                                                                      seq.int(1300, 9799),
                                                                      8000L, # Indig
                                                                      9998L,
                                                                      9999L),
                                                            na.rm = TRUE),
                valid = function(v) v %fin% c(0L, 1L,
                                              1201L, # English
                                              seq.int(1000, 1199),
                                              seq.int(1300, 9799),
                                              8000L, # Indig
                                              9998L,
                                              9999L),
                decoder = E348_decoder),

  "E350" = list(long_name = "Course_load",
                orig_name = "E350",
                mark_missing = function(v) v == 0,
                validate = function(v) all(between(v, 0, 10)),
                valid = function(v) v %fin% seq.int(0, 10)),
  "E354" = list(long_name = "Unit_of_study_cd",
                orig_name = "E354",
                mark_missing = never,
                validate = always,
                valid = every),

  "E355" = list(long_name = "Unit_of_study_completion_status",
                orig_name = "E355",
                mark_missing = never,
                ad_hoc_prepare = function(v) {v[v == 0L] <- NA_integer_; v},
                ad_hoc_validation_note = "Many 0s observed despite not being in dictionary. Cast to NA (int) via roll=TRUE.",
                validate = function(v) is.integer(v) && all(between(v, 0, 5), na.rm = TRUE),
                valid = function(v) v %fin% seq.int(1, 5),
                decoder = E355_decoder),

  "E358" = list(long_name = "CitizenResidentInd",
                orig_name = "E358",
                mark_missing = function(v) v == 9,
                validate = function(v) is.integer(v) && all(or(between(v, 1, 5),
                                                               v == 8 | v == 9)),
                valid = function(v) v %fin% c(seq.int(1, 5), 8, 9),
                decoder = E358_decoder),
  "E367" = list(long_name = "Prior_studies_exemption",
                orig_name = "E367",
                mark_missing = function(v) v == 0,
                validate = function(v) is.integer(v) && all(between(v, 0, 99)),
                valid = function(v) if (is.integer(v)) between(v, 0, 99) else v %fin% seq.int(0, 99)),
  "E368" = list(long_name = "Uni_providing_exemptorstatus",
                orig_name = "E368",
                mark_missing = function(v) v == 1 | v == 9999,
                validate = function(v) is.integer(v) && all(or(or(v == 1,
                                                                  between(v, 1000, 4999)),
                                                               or(between(v, 8001, 8004),
                                                                  v == 9999))),
                valid = function(v) or(or(v == 1,
                                          between(v, 1000, 4999)),
                                       or(between(v, 8001, 8004),
                                          v == 9999))),
  "E369" = list(long_name = "TER",
                orig_name = "E369",
                mark_missing = function(v) v >= 800 | v == 1L,
                ad_hoc_prepare = function(v) if_else(between(v, 2L, 29L), 998L, v),
                validate = function(v) is.integer(v) && all(or(or(v == 1L | between(v, 2, 29),
                                                                  between(v, 30L, 100L)),
                                                               v %fin% c(800L, 998L,
                                                                         999L))),
                ad_hoc_validation_note = "v == 800 appears for two entries (in 2007 and 2008): assumed to be NA. Otherwise missing if >= 998. Values 15 25 28 29 also present and cast as missing.",
                valid = function(v) or(or(v == 1L | between(v, 2, 29),
                                          between(v, 30L, 100L)),
                                       v %fin% c(800L, 998L,
                                                 999L))),
  "E381" = list(long_name = "Amt_paid_upfront",
                orig_name = "E381",
                mark_missing = never,
                validate = function(v) is.double(v) && all(between(v, 0, 99999999)),
                valid = function(v) between(v, 0, 99999999)),
  "E384" = list(long_name = "Tot_amt_charged",
                orig_name = "E384",
                mark_missing = never,
                # Note: uses dollars not cents
                validate = function(v) is.double(v) && all(between(v, 0, 999999)),
                valid = function(v) between(v, 0, 999999)),

  "E385" = list(long_name = "Tot_exemption_granted",
                orig_name = "E385",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 0, 99)),
                valid = function(v) if (is.integer(v)) between(v, 0, 99) else v %fin% seq.int(0, 99)),

  "E386" = list(long_name = "Disability",
                orig_name = "E386",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v %% 10, 0, 2) &
                                                              between(nth_digit_of(v, 8), 0, 2) &
                                                              between(nth_digit_of(v, 7), 0, 1) &
                                                              between(nth_digit_of(v, 6), 0, 1) &
                                                              between(nth_digit_of(v, 5), 0, 1) &
                                                              between(nth_digit_of(v, 4), 0, 1) &
                                                              between(nth_digit_of(v, 3), 0, 1) &
                                                              between(nth_digit_of(v, 2), 0, 2)),
                valid = function(v){
                  suppressWarnings({v <- as.integer(v)})
                  between(v %% 10, 0, 2) &
                    between(nth_digit_of(v, 8), 0, 2) &
                    between(nth_digit_of(v, 7), 0, 1) &
                    between(nth_digit_of(v, 6), 0, 1) &
                    between(nth_digit_of(v, 5), 0, 1) &
                    between(nth_digit_of(v, 4), 0, 1) &
                    between(nth_digit_of(v, 3), 0, 1) &
                    between(nth_digit_of(v, 2), 0, 2)
                },
                decoder = E386_decoder),
  "E390" = list(long_name = "Eligibility",
                orig_name = "E390",
                mark_missing = never,
                # is.logical --> accommodate all NA
                validate = function(v) AND(is.logical(v) || is.integer(v),
                                           all(between(v, 0, 3), na.rm = TRUE)),
                valid = function(v) v %fin% seq.int(0, 3)),
  "E392" = list(long_name = "Max_student_contr_ind",
                orig_name = "E392",
                mark_missing = never,
                # http://heimshelp.education.gov.au/sites/heimshelp/2008_data_requirements/2008dataelements/pages/392
                # Past elements had other values (which were treited in 2013: http://heimshelp.education.gov.au/sites/heimshelp/supporting_information/pages/392)
                validate = function(v) is.integer(v) && all(v %in% c(0, 1, 2, 3, 4, 6, 7, 5)),
                valid = function(v) v %fin% c(0, 1, 2, 3, 4, 6, 7, 5),
                decoder = E392_decoder),
  "E394" = list(long_name = "Course_name",
                orig_name = "E394",
                mark_missing = never,
                validate = always,
                valid = every),
  "E402" = list(long_name = "Surname",
                orig_name = "E402",
                mark_missing = never,
                validate = always,
                valid = every),
  "E403" = list(long_name = "Forename",
                orig_name = "E403",
                mark_missing = never,
                validate = always,
                valid = every),
  "E404" = list(long_name = "Other_name",
                orig_name = "E404",
                mark_missing = never,
                validate = always,
                valid = every),
  "E405" = list(long_name = "Name_title",
                orig_name = "E405",
                mark_missing = never,
                validate = always,
                valid = every),
  "E406" = list(long_name = "Postal_address_1",
                orig_name = "E406",
                mark_missing = never,
                validate = always,
                valid = every),
  "E407" = list(long_name = "Postal_address_2",
                orig_name = "E407",
                mark_missing = never,
                validate = always,
                valid = every),
  "E408" = list(long_name = "Staff_classification_type",
                orig_name = "E408",
                mark_missing = function(v) v == 999,
                validate = function(v) is.integer(v) && all(v %in% c(1,
                                                                     5,
                                                                     13,
                                                                     14,
                                                                     42,
                                                                     66,
                                                                     100,
                                                                     200,
                                                                     220,
                                                                     seq.int(201, 210),
                                                                     999)),
                valid = function(v){
                  v %fin% c(1,
                            5,
                            13,
                            14,
                            42,
                            66,
                            100,
                            200,
                            220,
                            seq.int(201, 210),
                            999)
                }),
  "E409" = list(long_name = "Postal_address_postcode",
                orig_name = "E409",
                mark_missing = function(v) if (is.integer(v)) v == 0L else v == "    ",
                validate = function(v) OR(is.integer(v) && all(between(v, 0, 9999)),
                                          all(between(as.integer(v), 0, 9999))),
                valid = function(v) between(v, 0, 9999)),
  "E412" = list(long_name = "Work_function_code",
                orig_name = "E412",
                mark_missing = function(v) v == 9,
                validate = function(v) is.integer(v) && all(v %in% c(1, 2, 3, 4, 9))),
  "E423" = list(long_name = "Current_salary",
                orig_name = "E423",
                mark_missing = function(v) v == 9,
                validate = function(v) is.integer(v) && all(between(v, 0, 800e3)),
                valid = function(v) if (is.integer(v)) between(v, 0, 800e3) else v %fin% seq.int(0, 800e3)),
  "E415" = list(long_name = "Reporting_yr",
                orig_name = "E415",
                mark_missing = never,
                validate = function(v) is.integer(v) && AND(all(between(v %% 10, 1, 2)),
                                                            all(between(v, 19891, 99992))),
                valid = function(v) and(between(v %% 10, 1, 2),
                                        between(v, 19891, 99992))),
  "E446" = list(long_name = "Variation_reason_cd",
                orig_name = "E446",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 1, 5)),
                valid = function(v) v %fin% seq.int(1, 5)),
  "E446A" = list(long_name = "Variation_reason_cd_init",
                 orig_name = "E446A",
                 mark_missing = never,
                 validate = function(v) is.character(v) && all(v %fin% c("N", "Y"), na.rm = TRUE),
                 ad_hoc_validation_note = "Field not present in data dictionary but values inferred as logical due to only non-missing values being Y, N.",
                 valid = function(v) v %fin% c("N", "Y"),
                 decoder = data.table(E446A = c("N", "Y"), Variation_reason_cd_init = c(FALSE, TRUE), key = "E446A")),
  "E455" = list(long_name = "is_Combined_course",
                orig_name = "E455",
                mark_missing = never,
                validate = function(v) OR(is.logical(v),
                                          AND(is.integer(v),
                                              all(between(v, 0, 1)))),
                valid = function(v) v == 0 | v == 1,
                decoder = function(DT){
                  DT[, is_Combined_course := as.logical(E455)]
                  DT[, E455 := NULL]
                  DT
                }),
  "E460" = list(long_name = "Prev_RTS_EFTSL",
                orig_name = "E460",
                mark_missing = never,
                validate = function(v){
                  v <- as.double(v)
                  v <- if_else(v > 10, v / 1000, v)
                  all(between(v, 0, 10))
                },
                ad_hoc_validation_note = "Put as double, despite data dictionary. Some values were nonetheless left in thousands, in particular Shafston Institute of Technology 4369 entries. Values above 10 assumed to be thousandths.",
                valid = function(v){
                  v <- as.double(v)
                  v <- if_else(v > 10, v / 1000, v)
                  between(v, 0, 10)
                }),
  "E461" = list(long_name = "FOE_cd",
                orig_name = "E461",
                mark_missing = never,
                validate = function(v) all(or(v == 0,
                                              between(v, 10000, 129999))),
                valid = function(v) if (is.integer(v)){
                  v == 0 | between(v, 10000, 129999)
                } else {
                  v %fin% c(0, seq.int(10e3, 129999))
                },
                decoder = {
                  out <- FOE_uniter[, .(FOE_cd, foename, foegrattan)]
                  setnames(out, c("FOE_cd", "foename", "foegrattan"), c("E461", "FOE_name", "FOE_Grattan"))
                  out[, FOE_cd_orig := E461]
                  out
                  }),
  "E462" = list(long_name = "FOE_supp_cd",
                orig_name = "E462",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(or(v == 0,
                                                               between(v, 10000, 129999))),
                valid = function(v) if (is.integer(v)){
                  v == 0 | between(v, 10e3, 129999)
                } else {
                  v %fin% c(0, seq.int(v, 10e3, 129999))
                }),
  "E463" = list(long_name = "Specialization_cd",
                orig_name = "E463",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 10000, 129999)),
                valid = function(v) if (is.integer(v)){
                  between(v, 10e3, 129999)
                } else {
                  v %fin% seq.int(10e3, 129999)
                },
                decoder = E463_decoder),

  "E464" = list(long_name = "Discipline_cd",
                orig_name = "E464",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 10000, 129999)),
                valid = function(v) if (is.integer(v)){
                  between(v, 10e3, 129999)
                } else {
                  v %fin% seq.int(10e3, 129999)
                },
                decoder = E464_decoder),

  "E465" = list(long_name = "change_due_xfer_to_research_course",
                orig_name = "E465",
                mark_missing = never,
                validate = function(v) all(v %in% c(1, 2, 3, 9)),
                valid = function(v) v %fin% c(1, 2, 3, 9),
                decoder = data.table(E465 = c(1L, 2L, 3L, 9L),
                                     change_due_xfer_to_research_course = c(0L, 1L, -1L, NA_integer_),
                                     key = "E465")),


  "E467" = list(long_name = "State_postal",
                orig_name = "E467",
                mark_missing = function(v) or(v == "   ", v == ""),
                validate = function(v) is.character(v) && all(trimws(v) %fin% c("NSW",
                                                                               "VIC",
                                                                               "QLD",
                                                                               "WA",
                                                                               "SA",
                                                                               "TAS",
                                                                               "NT",
                                                                               "ACT",
                                                                               "AAT")),
                valid = function(v) trimws(v) %fin% c("NSW",
                                                      "VIC",
                                                      "QLD",
                                                      "WA",
                                                      "SA",
                                                      "TAS",
                                                      "NT",
                                                      "ACT",
                                                      "AAT")),
  "E470" = list(long_name = "State_residential",
                orig_name = "E470",
                mark_missing = function(v) or(v == "   ", v == ""),
                validate = function(v) is.character(v) && all(trimws(v) %fin% c("NSW",
                                                                               "VIC",
                                                                               "QLD",
                                                                               "WA",
                                                                               "SA",
                                                                               "TAS",
                                                                               "NT",
                                                                               "ACT",
                                                                               "AAT")),
                valid = function(v) trimws(v) %fin% c("NSW",
                                                      "VIC",
                                                      "QLD",
                                                      "WA",
                                                      "SA",
                                                      "TAS",
                                                      "NT",
                                                      "ACT",
                                                      "AAT")),
  "E476" = list(long_name = "Commencing_location",
                orig_name = "E476",
                mark_missing = function(v) v == "99999",
                validate = function(v) is.character(v) && all(nchar(v) == 5L &
                                                                v %fin% c("00001",
                                                                          paste0("A", formatC(1:9998, width = 4, flag = "0")),
                                                                          "99999"))),
  "E477" = list(long_name = "Campus_postcode",
                orig_name = "E477",
                mark_missing = function(v) substr(v, 2, 5) == "9999",
                validate = function(v) is.character(v) && all(v %fin% c(paste0("X", c(1200:9299, 9999, c(1100, 9998))),
                                                                        paste0("A", formatC(1:9998, width = 4, flag = "0")),
                                                                        "99999")),
                ad_hoc_validation_note = "Two values 'X9998' and 'X1100' were also observed. Cast as overseas postcodes.",
                valid = function(v) v %fin% c(paste0("X", c(1200:9299, 9999)),
                                              paste0("A", formatC(1:9998, width = 4, flag = "0")),
                                              "99999"),
                decoder = function(DT){
                  DT[, Campus_postcode := if_else(grepl("^A", E477, perl = TRUE),
                                                  gsub("^A", "", E477, perl = TRUE),
                                                  NA_character_)]
                  DT[, E477 := NULL]
                  DT
                }),

  "E459" = list(long_name = "Campus_location",
                orig_name = "E459",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 1L, 2L)),
                decoder = data.table(E459 = c(1L, 2L),
                                     Campus_location = c("Australia", "Offshore"),
                                     key = "E459")),

  "E486" = list(long_name = "Suburb",
                orig_name = "E486",
                mark_missing = never,
                validate = always),
  "E487" = list(long_name = "Scholarship_type_cd",
                orig_name = "E487",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(v %in% c(0, 1, 2, 6, 7))),
  "E488" = list(long_name = "CHESSN",
                orig_name = "E488",
                mark_missing = function(v) v == 0,
                ad_hoc_validation_note = "Treated as 64-bit integer. Import Z's as NA (only value that requires char).",
                validate = function(v) is.integer(v) || is.integer64(v),
                post_fst = function(DT){
                  setattr(DT[["CHESSN"]], "class", "integer64")
                }),
  "E489" = list(long_name = "Census_date",
                orig_name = "E489",
                mark_missing = never,
                validate = function(v) all(is.Date(v)),
                valid = function(v) is.Date(v),
                decoder = function(DT){
                  DT[, "Census_date" := ymd(E489)]
                  DT[, E489 := NULL]
                },
                post_fst = function(DT){
                  setattr(DT[["Census_date"]], "class", "Date")
                }),
  "E490" = list(long_name = "Student_status_cd",
                orig_name = "E490",
                mark_missing = never,
                validate = function(v) all(v %fin% E490_decoder[["E490"]]),
                valid = function(v) v %fin% E490_decoder[["E490"]],
                decoder = E490_decoder),
  # Ittima email 2017-02-07
  "U490" = list(long_name = "Student_status_abbrev",
                orig_name = "U490",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(or(between(v, 1, 4),
                                                               between(v, 8, 9))),
                decoder = U490_decoder),
  "E493" = list(long_name = "Max_edu_level_b4_start",
                orig_name = "E493",
                mark_missing = function(v) between(v, 0, 19999),
                ad_hoc_prepare = function(v) if_else(and(between(v, 20e3, 119999),
                                                         (v %% 10e3) <= 1899), # i.e. year but not plausible
                                                                               # N.B. we can't assume 07 -> 2007 because that
                                                                               # would admit or not exclude implausible values (1811) etc
                                                                               # likely some are MMDD dates of birth rather than YYYY
                                                     (v %/% 10000L) * 10000L + 9999L, # force year component to be missing
                                                     v),
                ad_hoc_validation_note = "If value between 20000 and 119999, and the year component is \\leq 1899, then we force year component to be missing. Treat values \\leq 19999 as missing.",
                validate = function(v) is.integer(v) && all(or(between(v, 0, 19999),
                                                               and(between(v %/% 10000, 2, 11),
                                                                   or(or(between(v %% 10000, 1900, 2017),
                                                                         (v %% 10000) %fin% c(0, 9999)),
                                                                      v == 90000)))),
                valid = function(v) or(between(v, 0, 19999),
                                       and(between(v %/% 10000, 2, 11),
                                           or(or(between(v %% 10000, 1900, 2017),
                                                 (v %% 10000) %fin% c(0, 9999)),
                                              v == 90000))),
                decoder = function(DT){
                  Edu_level <-
                    data.table(E493 = as.integer(c(2, 3, 4, 5, 7, 8, 9, 10, 11) * 10e3),
                               Max_edu_level_ante = c("Complete Postgrad",
                                                      "Complete Bachelor",
                                                      "Complete Sub-degree",
                                                      "Incomplete HE course",
                                                      "Complete high school",
                                                      "Other qualification",
                                                      "No prior edu",
                                                      "Complete VET",
                                                      "Incomplete VET"),
                               key = "E493")

                  DT[, Year_Max_edu_level_ante := if_else(E493 > 19999L, E493 %% 10000L, NA_integer_)]
                  setkeyv(DT, "E493")
                  out <- Edu_level[DT, roll = -Inf]
                  setkey(out, NULL)
                  out[, Year_Max_edu_level_ante := if_else(Year_Max_edu_level_ante == 9999, NA_integer_, Year_Max_edu_level_ante)]
                  out
                }),
  "E495" = list(long_name = "Indic_student_contr_amt",
                orig_name = "E495",
                mark_missing = function(v) v == 99999L,
                validate = function(v) is.integer(v) && all(between(v, 0, 99999)),
                valid = function(v) if (is.integer(v)){
                  between(v, 0, 99999)
                } else {
                  v %fin% seq.int(0, 99999)
                }),
  "E496" = list(long_name = "Indic_tuition_fee",
                orig_name = "E496",
                mark_missing = function(v) v == 99999L,
                validate = function(v) is.integer(v) && all(between(v, 0, 99999)),
                valid = function(v) if (is.integer(v)){
                  between(v, 0, 99999)
                } else {
                  v %fin% seq.int(0, 99999)
                }),
  "E497" = list(long_name = "Entry_cutoff_CSP",
                orig_name = "E497",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v) && all(between(v, 0, 9995)),
                valid = function(v) v %fin% seq.int(0, 9995)),
  "E498" = list(long_name = "Entry_cutoff_domestic",
                orig_name = "E498",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v) && all(between(v, 0, 9995))),
  "E500" = list(long_name = "Overseas_student_fee_",
                orig_name = "E500",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 0, 999999)),
                ad_hoc_validation_note = "Some fees exceed 100,000 (not by much). Left as-is.",
                valid = function(v) if (is.integer(v)){
                  between(v, 0, 999999)
                } else {
                  v %fin% seq.int(0, 999999)
                }),

  "E521" = list(long_name = "OS_HELP_Study_period_start_date",
                orig_name = "E521",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v),
                valid = every),
  "E522" = list(long_name = "Cohort_year",
                orig_name = "E522",
                mark_missing = function(v) between(v, 0, 1),
                ad_hoc_validation_note = "Not used due to excessive invalid codes.",
                validate = always,
                valid = every,
                decoder = function(DT) DT[, E522 := NULL]),
                # http://heimshelp.education.gov.au/sites/heimshelp/2005_data_requirements/2005dataelements/pages/522
                # validate = function(v) is.integer(v) && or(between(v, 0, 1),
                #                                            between(v, 2005, 2009)),
                # valid = function(v) v %fin% c(0, 1, seq.int(2005, 2009))),
  "E523" = list(long_name = "Qld_entry_cut_off_CSP",
                orig_name = "E523",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v),
                valid = every),
  "E524" = list(long_name = "Qld_entry_cut_off_domestic",
                orig_name = "E524",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v),
                valid = every),
  "E527" = list(long_name = "HELP_debt_incurral_date",
                orig_name = "E527",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v),
                valid = every),
  "E528" = list(long_name = "OS_HELP_Payment_amt",
                orig_name = "E528",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v) && all(between(v, 0, 99999999)),
                valid = function(v) if (is.integer(v)){
                  between(v, 0, 99999999)
                } else {
                  v %fin% seq.int(0, 99999999)
                }),
  "E529" = list(long_name = "Loan_fee",
                orig_name = "E529",
                mark_missing = never,
                validate = function(v) is.double(v) && all(between(v, 0, 99999999 / 10)),
                ad_hoc_validation_note = "Due to apparent upstream reencoding, assumed to be in dollars.",
                valid = function(v) between(v, 0, 99999999 / 10)),
  "E533" = list(long_name = "Course_of_study_cd",
                orig_name = "E533",
                mark_missing = function(v) if (is.numeric(v)) v == 0L else v == "0000000000",
                validate = always,
                valid = every),
  "E534" = list(long_name = "Course_start_date",
                orig_name = "E534",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(is.YearMonth(v)),
                valid = function(v) is.YearMonth(v),
                decoder = function(DT){
                  DT[, Course_start_date := ymd(E534 * 100 + 1)]
                  DT[, E534 := NULL]
                  DT
                }),
  "E536" = list(long_name = "Course_fee_type",
                orig_name = "E536",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v) && all(v %in% seq.int(0, 3)),
                valid = function(v) v %fin% seq.int(0, 3)),
  "E550" = list(long_name = "Ref_year",
                orig_name = "E550",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 1987, 2099)),
                valid = function(v) if (is.integer(v)){
                  between(v, 1987, 2099)
                } else {
                  v %fin% seq.int(1987, 2099)
                }),
  "E551" = list(long_name = "SummerWinter_school_ind",
                orig_name = "E551",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 1, 3)),
                valid = function(v) v %fin% seq.int(1, 3),
                decoder = E551_decoder),

  "E558" = list(long_name = "HELP_debt_amt",
                orig_name = "E558",
                mark_missing = never,
                # validate = function(v) is.integer(v) && all(between(v, 0, 99999999)),
                ad_hoc_validation_note = "NAs not meant to be permitted but present, left as-is. Due to upstream reencoding, interpretable as dollars.",
                validate = function(v) is.double(v) && all(between(v, 0, 99999999 / 100), na.rm = TRUE),
                valid = function(v) between(v, 0, 99999999 / 100)),
  "E560" = list(long_name = "Credit_used_value",
                orig_name = "E560",
                mark_missing = function(v) v == 0,
                # Should be:
                # validate = function(v) is.integer(v) && all(between(v, 0, 9999)),
                # but due to upstream reencoding:
                ad_hoc_prepare = function(v) if_else(v > 10, v / 1000, v),
                ad_hoc_validation_note = "Mixture of thousandths and doubles. Anything above 10 assumed to be thousandths",
                validate = function(v) is.double(v) && all(between(v, 0, 10), na.rm = TRUE),
                valid = function(v) between(v, 0, 10)),

  "E561" = list(long_name = "Prior_creditable_study_dets",
                orig_name = "E561",
                mark_missing = function(v) v == 0,
                validate = function(v) is.integer(v) && all(v %fin% (100L * seq.int(0, 6))),
                valid = function(v) (v %fin% (100L * seq.int(0, 6)))),
  "E562" = list(long_name = "FOE_prior_creditable_VET_study",
                orig_name = "E562",
                mark_missing = function(v) v == 0,
                ad_hoc_prepare = function(v) if_else(v %fin% c(1, 2), v * 100L, v),
                validate = function(v) is.integer(v) && all(or(v == 0,
                                                               between(v, 100, 1299))),
                valid = function(v) or(v == 0, between(v, 100, 1299)),
                decoder = E562_decoder),
  "E563" = list(long_name = "Edu_level_creditable_VET_study",
                orig_name = "E563",
                mark_missing = function(v) v == 0 | v == 999,
                validate = function(v) is.integer(v) && all(v %fin% c(0:2,
                                                                      411, 412, 415,
                                                                      421:423,
                                                                      511:516,
                                                                      521:525,
                                                                      999)),
                valid = function(v) v %fin% c(0:2,
                                              411, 412, 415,
                                              421:423,
                                              511:516,
                                              521:525,
                                              999)),
  "E564" = list(long_name = "Provider_type_where_VET_undertaken",
                orig_name = "E564",
                mark_missing = function(v) v == 0,
                validate = function(v) is.integer(v) && all(v %fin% c(0, 10, 19,
                                                                      20, 21, 29,
                                                                      90)),
                valid = function(v) v %fin% c(0, 10, 19,
                                              20, 21, 29,
                                              90)),
  "E565" = list(long_name = "Credit_offered_as_EFTSL",
                orig_name = "E565",
                mark_missing = never,
                validate = function(v) AND(is.integer(v) || is.double(v),
                                           all(between(v, 0, 9999))),
                ad_hoc_validation_note = "Mixture of doubles and integers in EFTSL. Anything above 10 assumed to be thousandths.",
                valid = function(v) if (is.integer(v)){
                  between(v, 0, 9999)
                } else {
                  if_else(v > 10,
                          v %fin% seq.int(0, 9999),
                          between(v, 0, 10))
                },
                decoder = function(DT){
                  DT[, Credit_offered_as_EFTSL := if_else(E565 > 10, E565 / 1000, as.double(E565))]
                  DT[, "E565" := NULL]
                }),
  "E566" = list(long_name = "Credit_offered_as_EFTSL_by",
                orig_name = "E566",
                mark_missing = function(v) v %fin% c(0L, 1L, 3L, 10L, 2L, 4L, 7L, 5L, 9998L, 9999L),
                ad_hoc_validation_note = "c(1L, 3L, 10L, 2L, 4L, 7L, 5L, 9998L) were also observed. Assumed to be missing.",
                validate = function(v) is.integer(v) && all(or(v %fin% c(0L, 1L, 3L, 10L, 2L, 4L, 7L, 5L, 9998L, 9999L),
                                                               between(v, 1000, 7997))),
                valid = function(v) if (is.integer(v)){
                  or(v %fin% c(0L, 1L, 3L, 10L, 2L, 4L, 7L, 5L, 9998L, 9999L),
                     between(v, 1000, 7997))
                } else {
                  v %fin% c(0, 9999, seq.int(1000, 7997))
                }),
  "E567" = list(long_name = "Scholarship_variation_reason",
                orig_name = "E567",
                mark_missing = never,
                validate = function(v) is.integer(v) && all(between(v, 0, 2)),
                valid = function(v) v %fin% seq.int(0, 2)),
  "E568" = list(long_name = "Scholarship_address_postcode",
                orig_name = "E568",
                mark_missing = never,
                validate = always,
                valid = every),
  "E569" = list(long_name = "Operation_type_of_overseas_campus",
                orig_name = "E569",
                mark_missing = function(v) v == 0,
                validate = function(v) is.integer(v) && all(between(v, 0, 2)),
                valid = function(v) v %fin% seq.int(0, 2)),


  "E572" = list(long_name = "Year_left_school",
                orig_name = "E572",
                mark_missing = function(v) or(v == 0L | v == 1L,
                                              v >= 9997L),
                validate = function(v) is.integer(v) && all(between(v, 0, 9999)),
                valid = function(v) if (is.integer(v)){
                  between(v, 0, 9999)
                } else {
                  v %fin% seq.int(0, 9999)
                }),
  "E573" = list(long_name = "Education_parent1",
                orig_name = "E573",
                mark_missing = function(v) v %in% c(1, 49, 98, 99),
                validate = function(v) is.integer(v) && all(v %fin% c(c(1, 49, 98, 99, 59),
                                                                      20, 21, 22, 23, 24, 25, 26,
                                                                      40, 41, 42, 43, 44, 45, 46)),
                valid = function(v) v %fin% c(c(1, 49, 98, 99, 59),
                                              20, 21, 22, 23, 24, 25, 26,
                                              40, 41, 42, 43, 44, 45, 46),
                decoder = function(DT){
                  edu_decoder <- data.table(d1 = c(0:6),
                                            Education_parent1 = c("Postgrad",
                                                                  "Bachelor",
                                                                  "Other post-school",
                                                                  "Year 12",
                                                                  "Not Year 12",
                                                                  "Year 10",
                                                                  "Not Year 10"),
                                            key = "d1")
                  DT[, d1 := E573 %% 10]
                  DT <- merge(DT, edu_decoder, by = "d1", all.x = TRUE)
                  DT[, d1 := NULL]
                  DT
                }),
  "E574" = list(long_name = "Education_parent2",
                orig_name = "E574",
                mark_missing = function(v) v %in% c(1, 49, 98, 99, 59),
                validate = function(v) is.integer(v) && all(v %fin% c(c(1, 49, 98, 99, 59),
                                                                      20, 21, 22, 23, 24, 25, 26,
                                                                      40, 41, 42, 43, 44, 45, 46)),
                valid = function(v) v %fin% c(c(1, 49, 98, 99, 59),
                                              20, 21, 22, 23, 24, 25, 26,
                                              40, 41, 42, 43, 44, 45, 46),
                decoder = function(DT){
                  edu_decoder <- data.table(d1 = c(0:6),
                                            Education_parent2 = c("Postgrad",
                                                                  "Bachelor",
                                                                  "Other post-school",
                                                                  "Year 12",
                                                                  "Not Year 12",
                                                                  "Year 10",
                                                                  "Not Year 10"),
                                            key = "d1")
                  DT[, d1 := E574 %% 10]
                  DT <- merge(DT, edu_decoder, by = "d1", all.x = TRUE)
                  DT[, d1 := NULL]
                  DT
                }),
  "E578" = list(long_name = "Completion_percentage",
                orig_name = "E578",
                mark_missing = function(v) v == 100L,
                validate = function(v) is.integer(v) && all(between(v, 0, 100), na.rm = TRUE),
                valid = function(v) if (is.integer(v)){
                  between(v, 0, 100)
                } else {
                  v %fin% seq.int(0, 100)
                }),
  "E579" = list(long_name = "Joint_degree_partner_HE_Provider_cd",
                orig_name = "E579",
                mark_missing = function(v) v == 0L,
                validate = function(v) is.integer(v) && all(or(v == 0L,
                                                               between(v, 1000, 7997))),
                valid = function(v) v %fin% c(0, seq.int(1000, 7997))),
  "E582" = list(long_name = "OS_HELP_Language_studied",
                orig_name = "E582",
                mark_missing = function(v) v == 9999,
                validate = function(v) is.integer(v) && all(or(between(v, 1000, 9799),
                                                               v == 9999)),
                valid = function(v) v %fin% c(9999, seq.int(1000, 7997))),
  "E702" = list(long_name = "Aust_Yr12_result",
                orig_name = "E702",
                mark_missing = function(v) v == 10,
                validate = function(v) is.integer(v) && all(between(v, 1, 10)),
                valid = function(v) v %fin% seq.int(1, 10)),
  "E710" = list(long_name = "IB_score",
                orig_name = "E710",
                mark_missing = function(v) v == 99,
                validate = function(v) is.integer(v) && all(or(between(v, 21, 45),
                                                               v == 99)),
                valid = function(v) v %fin% c(seq.int(21, 45), 99)),
  "E730" = list(long_name = "Prior_postgrad_course_year",
                orig_name = "E730",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 39999))),
                valid = function(v) v %fin% c(9999, 10000, seq.int(20e3, 39999))),

  "E731" = list(long_name = "Prior_degree",
                orig_name = "E731",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 39999))),
                valid = function(v)v %fin% c(9999, 10000, seq.int(20e3, 39999))),
  "E732" = list(long_name = "Prior_subdegree_course_HE",
                orig_name = "E732",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 39999))),
                valid = function(v) v %fin% c(9999, 10000, seq.int(20e3, 39999))),
  "E733" = list(long_name = "Prior_subdegree_course_VET",
                orig_name = "E733",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 39999))),
                valid = function(v) v %fin% c(9999, 10000, seq.int(20e3, 39999))),
  "E734" = list(long_name = "Prior_VET_award_course",
                orig_name = "E734",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 39999))),
                valid = function(v) v %fin% c(9999, 10000, seq.int(20e3, 39999))),
  "E735" = list(long_name = "Prior_secondary_edu_VET_course",
                orig_name = "E735",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 29999))),
                valid = function(v) v %fin% c(9999, 10000, seq.int(20e3, 29999))),
  "E736" = list(long_name = "Prior_secondary_edu_school_course",
                orig_name = "E736",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 29999))),
                valid = function(v) v %fin% c(9999, 10000, seq.int(20e3, 29999))),
  "E737" = list(long_name = "Prior_other_quals",
                orig_name = "E737",
                mark_missing = function(v) v == 10000 | v == 9999,
                validate = function(v) all(or(v %in% c(9999, 10000),
                                              between(v, 20000, 29999))),
                valid = function(v) v %fin% c(9999, 10000, seq.int(20e3, 29999))),
  "E913" = list(long_name = "Age_EOY",
                orig_name = "E913",
                ad_hoc_validation_note = "High ages most likely due to high DOBs. Left as-is.",
                mark_missing = function(v) v == 0 | !between(v, 0, 115),
                validate = function(v) is.integer(v) && all(v == 0 | between(v, 0, 115)),
                valid = function(v) v %fin% c(0, seq.int(0, 115))),

  "E919" = list(long_name = "State_permanent_home",
                orig_name = "E919",
                mark_missing = function(v) v == 9, # not idempotent
                validate = function(v) is.integer(v) && all(between(v, 0, 9), na.rm = TRUE),
                valid = function(v) v %fin% seq.int(0, 9),
                decoder = E919_decoder),
  "E920" = list(long_name = "State_term_location",
                orig_name = "E920",
                mark_missing = function(v) v == 9, # not idempotent
                validate = function(v) is.integer(v) && all(between(v, 0, 9), na.rm = TRUE),
                valid = function(v) v %fin% seq.int(0, 9),
                decoder = E920_decoder),
  "E922" = list(long_name = "Commencing_student_ind",
                orig_name = "E922",
                mark_missing = never,
                validate = function(v) OR(is.character(v) && all(v %in% c("1", "2")),
                                          is.integer(v) && all(between(v, 1L, 2L))),
                valid = function(v) if (is.character(v)){
                  v %fin% c("1", "2")
                } else {
                  v %fin% c(1, 2)
                },
                decoder = E922_decoder),
  "E931" = list(long_name = "Aggreg_EFTSL",
                orig_name = "E931",
                mark_missing = never,
                # original was:
                # validate = function(v) is.integer(v) && all(between(v, 0, 99999)),
                # but due to upstream re-encoding:
                validate = function(v) is.double(v) && all(between(v, 0, 9.9999)),
                valid = function(v) between(v, 0, 9.9999)),
  "E996" = list(long_name = "State_of_institution",
                orig_name = "E996",
                mark_missing = never,
                validate = function(v) is.character(v) && all(trimws(v) %fin% c("NSW",
                                                                                "VIC",
                                                                                "QLD",
                                                                                "WA",
                                                                                "SA",
                                                                                "TAS",
                                                                                "NT",
                                                                                "ACT",
                                                                                "MUL",
                                                                                "OS"), na.rm = TRUE),
                valid = function(v) trimws(v) %fin% c("NSW",
                                                      "VIC",
                                                      "QLD",
                                                      "WA",
                                                      "SA",
                                                      "TAS",
                                                      "NT",
                                                      "ACT",
                                                      "MUL",
                                                      "OS")),
  "E997" = list(long_name = "Participation_age",
                orig_name = "E997",
                mark_missing = function(v) v %fin% c(0, 2) | !between(v, 0, 99),
                ad_hoc_validation_note = "Some ages 115 and 11: guessing 11 is genuine. Anything below 2 and above 100 cast to missing.",
                validate = function(v) is.integer(v) && all(v %fin% c(0, 2, seq.int(0, 115)), na.rm = TRUE),
                valid = function(v) v %fin% c(0, 2, seq.int(0, 115))),

  "ses_cd" = list(long_name = "ses_cd",
                     orig_name = "SES_CD",
                     mark_missing = function(v) v == "x",
                     validate = function(v) all(v %fin% c("h", "m", "l", "x")),
                     valid = function(v) v %fin% c("h", "m", "l", "x"),
                     decoder = function(DT){
                       DT[, CD_SES := factor(ses_cd,
                                             levels = c("l", "m", "h"),
                                             labels = c("Low", "Medium", "High"),
                                             ordered = TRUE)]
                       DT[, c("ses_cd") := NULL]
                       DT
                     }),
  "A_SES2011" = list(long_name = "SES_2011",
                     orig_name = "A_SES2011",
                     mark_missing = function(v) v == "x",
                     validate = function(v) all(v %fin% c("h", "m", "l", "x")),
                     valid = function(v) v %fin% c("h", "m", "l", "x"),
                     decoder = function(DT){
                       DT[, SES_2011 := factor(A_SES2011,
                                               levels = c("l", "m", "h"),
                                               labels = c("Low", "Medium", "High"),
                                               ordered = TRUE)]
                       DT[, c("A_SES2011") := NULL]
                       DT
                     })
) -> heims_data_dict

# The dictionary's callbacks are closures created here, in the global
# environment, but they are called from (and documented for use with) the
# installed package, where they need to see heims' imports -- setnames(),
# between(), %fin%, etc. Since heims only Imports data.table, the global
# environment no longer reaches those names, so bind every callback to the
# heims namespace before serializing. save() stores that as a namespace
# *reference*, so the closures resolve against heims' imports on load,
# whether or not data.table is attached.
heims_data_dict <- lapply(heims_data_dict, function(entry) {
  is_fun <- vapply(entry, is.function, logical(1))
  entry[is_fun] <- lapply(entry[is_fun], `environment<-`, asNamespace("heims"))
  entry
})

usethis::use_data(heims_data_dict, overwrite = TRUE, compress = "bzip2", version = 2)
