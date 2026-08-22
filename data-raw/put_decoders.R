library(data.table)
library(magrittr)
library(heims)
library(hutils)


setuniquekey <- function(DT, ...){
  setkey(DT, ...)
  if (has_unique_key(DT)){
    return(DT)
  } else {
    stop("Key is not unique.")
  }
}


E089_decoder <-
  data.table(E089 = c(1L, 2L),
             is_1st_completion_record = c(TRUE, FALSE),
             key = "E089")

E095_decoder <-
  fread("./data-raw/decoders/E095-decoder.csv") %>%
  setuniquekey(E095)

E306_decoder <-
  HE_Provider_decoder <-
  fread("./data-raw/decoders/Providers-by-code-Appendix-A.csv") %>%
  setnames("Provider Code", "E306") %>%
  setnames("Provider Name", "HE_Provider_name") %>%
  setnames("Provider Type", "HE_Provider_type") %>%
  .[, TableA := any(grepl("Table A", HE_Provider_type)), by = HE_Provider_name] %>%
  unique(by = "E306", fromLast = TRUE) %>%
  setuniquekey(E306)

E310_decoder <-
  fread("./data-raw/decoders/E310-decoder.csv") %>%
  .[, Course_type := substr(Course_type, 0, 38)] %>%
  .[, Course_type := factor(Course_type, levels = unique(.$Course_type), ordered = TRUE)] %>%
  .[, Course_type_short := factor(Course_type_short, levels = unique(.$Course_type_short), ordered = TRUE)] %>%
  setkey(E310)

E312_decoder <-
  fread("./data-raw/decoders/E312-decoder.csv", na.strings = "") %>%
  .[, .(E312, Special_course)] %>%
  setkey(E312)

E316_decoder <-
  fread("./data-raw/decoders/E316-decoder.csv") %>%
  setkey(E316)

E327_decoder <-
  fread("./data-raw/decoders/E327_decoder.csv") %>%
  setkey(E327)

E329_decoder <-
  fread("./data-raw/decoders/E329-ModeAttendance-decoder.tsv") %>%
  setkey(E329)

E330_decoder <-
  fread("./data-raw/decoders/E330-decoder.csv") %>%
  setkey(E330)

E331_decoder <-
  data.table(E331 = c(1, 2, 3),
             Simult_enrol = c(FALSE, TRUE, TRUE),
             Major_course = c(NA, TRUE, FALSE),
             key = "E331")

E337_decoder <-
  fread("./data-raw/decoders/E337-decoder.txt") %>%
  setkey(E337)

force_integer <- function(x){
  suppressWarnings(as.integer(x))
}

E346_decoder <-
  fread("./data-raw/decoders/ABS-country-codes.csv", na.strings = "", header = FALSE) %>%
  setnames(1:4, paste0("V", 1:4)) %>%
  .[, Region_code := as.integer(force_integer(V1) * 1000)] %>%
  .[, Country_broad_code := as.integer(force_integer(V2) * 100)] %>%
  .[] %>%
  .[, Region_name := if_else(!is.na(V1), V2, NA_character_)] %>%
  .[, Country_name := coalesce(V4, V3, V2)] %>%
  .[, Region_name := zoo::na.locf(Region_name, na.rm = FALSE)] %>%
  .[, Country_code := force_integer(V3)] %>%
  .[, Country_code := coalesce(Region_code, Country_broad_code, force_integer(V3))] %>%
  .[, .(Country_code, Country_name, Region_name)] %>%
  .[complete.cases(.)] %>%
  .[, E346 := Country_code] %>%
  rbind(data.table(E346 = c(9998L, 9999L),
                   Country_code = c(9998L, 9999L),
                   Country_name = c(NA_character_, NA_character_),
                   Region_name = c(NA_character_, NA_character_))) %>%
  rbind(fread("./data-raw/decoders/ABS-country-code-2006-2nd-edn.csv"),
        use.names = TRUE, fill = TRUE) %>%
  rbind(fread("./data-raw/decoders/ABS-country-code-1998-suppl.csv"),
        use.names = TRUE, fill = TRUE) %>%
  .[, Country_code := NULL] %>%
  .[, Country_of_birth := trimws(gsub("[\\(,].*$", "", Country_name))] %>%
  .[, .(E346, Country_of_birth, Region_name)] %>%
  .[, Country_of_birth := if_else(Country_of_birth %in% c("England", "Wales", "Scotland"),
                                  "United Kingdom",
                                  Country_of_birth)] %>%
  setkey(E346) %>%
  .[]

E348_decoder <-
  fread("./data-raw/decoders/E348-decoder.csv", na.strings = "") %>%
  setkey(E348) %>%
  # Extended languages: Choose the first.
  unique(by = key(.))

E355_decoder <-
  fread("./data-raw/decoders/E355-decoder.csv") %>%
  setkey(E355)

E358_decoder <-
  fread("./data-raw/decoders/E358-decoder.txt") %>%
  setkey(E358)

E386_decoder <-
  # sort(unique(enrols_2015$E386)
  CJ(E386 = c(0L, 2L, 10000000L, 10000001L, 10000002L, 10000010L, 10000011L,
              10000012L, 10000100L, 10000101L, 10000102L, 10000110L, 10000111L,
              10000112L, 10001000L, 10001001L, 10001002L, 10001011L, 10001012L,
              10001101L, 10001102L, 10001111L, 10001112L, 10010000L, 10010001L,
              10010002L, 10010011L, 10010012L, 10010101L, 10010102L, 10010111L,
              10010112L, 10011001L, 10011002L, 10011011L, 10011012L, 10011101L,
              10011102L, 10011111L, 10011112L, 10100000L, 10100001L, 10100002L,
              10100010L, 10100011L, 10100012L, 10100101L, 10100102L, 10100111L,
              10100112L, 10101001L, 10101002L, 10101011L, 10101012L, 10101101L,
              10101102L, 10101111L, 10101112L, 10110001L, 10110002L, 10110011L,
              10110012L, 10110101L, 10110102L, 10110111L, 10110112L, 10111001L,
              10111002L, 10111011L, 10111012L, 10111101L, 10111102L, 10111111L,
              10111112L, 11000000L, 11000001L, 11000002L, 11000011L, 11000012L,
              11000101L, 11000102L, 11000111L, 11000112L, 11001001L, 11001002L,
              11001011L, 11001012L, 11001101L, 11001102L, 11001111L, 11001112L,
              11010001L, 11010002L, 11010011L, 11010012L, 11010101L, 11010102L,
              11010111L, 11010112L, 11011001L, 11011002L, 11011011L, 11011012L,
              11011101L, 11011102L, 11011111L, 11011112L, 11100001L, 11100002L,
              11100011L, 11100012L, 11100101L, 11100102L, 11100111L, 11100112L,
              11101001L, 11101002L, 11101011L, 11101101L, 11101102L, 11101111L,
              11110001L, 11110002L, 11110011L, 11110012L, 11110101L, 11110102L,
              11110111L, 11110112L, 11111001L, 11111002L, 11111011L, 11111012L,
              11111101L, 11111102L, 11111111L, 11111112L, 20000000L, 20000001L,
              20000002L),
     digits = 1:8,
     v = 0:2) %>%
  .[E386 == 2L, E386 := 20000000L] %>%
  merge(data.table(digits = 1:8,
                   disability = paste0(c("any", "hearing", "learning", "mobility", "visual", "medical", "other", "wants_services"), "_disability")),
        by = "digits") %>%
  merge(data.table(v = 0:2,
                   answer = c(NA, TRUE, FALSE)), by = "v") %>%
  .[nth_digit_of(E386, n = 9 - digits) == v] %>%
  .[, .(E386, disability, answer)] %>%
  unique %>%
  dcast.data.table(E386 ~ disability) %>%
  setkey(E386)

E392_decoder <-
  data.table(E392 = c(0, 3, 7, 6, 5),
             Max_student_contr = c("Not exempt", "Max Cth contrib.", "Max Cth contrib.", "Max Cth contrib.", "Max Cth contr. for nursing"),
             key = "E392")

FOE_uniter <-
  fread("./data-raw/foegrattan-ittima.csv") %>%
  setnames("foecode", heims_data_dict$E461$long_name) %>%
  setkeyv(heims_data_dict$E461$long_name)

E461_decoder <-
  fread("./data-raw/decoders/E461-FOE-decoder.tsv") %>%
  setkey(E461)

E463_decoder <-
  FOE_uniter %>%
  copy %>%
  .[, .(E463 = FOE_cd,
        Specialization = foename)] %>%
  setkey(E463)

E464_decoder <-
  FOE_uniter %>%
  copy %>%
  .[, .(E464 = FOE_cd,
        Discipline = foename)] %>%
  setkey(E464)


# Abbreviated because actuals are ridic
E490_decoder <-
  fread("./data-raw/decoders/E490-decoders.txt") %>%
  setnames("CODE", "E490") %>%
  setnames("Meaning", "Student_status_cd") %>%
  .[,
    .(E490,
      Paid_upfront = if_else(grepl("^Paid( the)? full", Student_status_cd, perl = TRUE),
                             TRUE,
                             if_else(grepl("^Deferred", Student_status_cd, perl = TRUE),
                                     FALSE,
                                     NA)))] %>%
  setkey(E490)

U490_decoder <-
  fread("./data-raw/decoders/U490-decoder.tsv") %>%
  setkey(U490)

E551_decoder <-
  fread("./data-raw/decoders/E551-decoder.tsv") %>%
  setkey(E551)

E562_decoder <-
  fread("./data-raw/decoders/E562-decoder.txt") %>%
  setkey(E562)

E919_decoder <-
  fread("./data-raw/decoders/E919-decoder.txt", na.strings = "") %>%
  setkey(E919)

E920_decoder <-
  fread("./data-raw/decoders/E919-decoder.txt", na.strings = "") %>%
  setnames("E919", "E920") %>%
  setnames("State_permanent_home", "State_term_location") %>%
  setkey(E920)

E922_decoder <-
  data.table(E922 = c(1L, 2L),
             Commencing_student = c(TRUE, FALSE),
             key = "E922")

lapply(mget(ls(pattern = "decoder")), function(dt){
  if (!has_unique_key(dt)){
    print(names(dt))
    stop("DT has non-unique key")
  }})

usethis::use_data(E089_decoder,
                   E095_decoder,
                   E306_decoder, HE_Provider_decoder,
                   E310_decoder,
                   E312_decoder,
                   E316_decoder,
                   E327_decoder,
                   E329_decoder,
                   E330_decoder,
                   E331_decoder,
                   E337_decoder,
                   E346_decoder,
                   E348_decoder,
                   E355_decoder,
                   E358_decoder,
                   E386_decoder,
                   E392_decoder,
                   E461_decoder,
                   E463_decoder,
                   E464_decoder,
                   E490_decoder,
                   U490_decoder,
                   E551_decoder,
                   E562_decoder,
                   E919_decoder,
                   E920_decoder,
                   E922_decoder,
                   FOE_uniter,
                   internal = FALSE, overwrite = TRUE)

all_tables_char <- ls(pattern = "decoder")
all_tables <- mget(all_tables_char)
provide.dir("tsv")
for (i in seq_along(all_tables_char)) {
  if (is.data.table(all_tables[[i]])) {
    fwrite(all_tables[[i]],
           file.path("tsv", paste0(all_tables_char[[i]], ".tsv")),
           sep = "\t")
  }
}

# The dictionary depends on the decoders themselves
source("./data-raw/put-heims_data_dict.R")

