#' Validate HEIMS elements
#' @name element_validation
#' @description Return TRUE or FALSE on whether or not each variable in a data.table complies with the HEIMS code limits
#' @param DT The data.table whose variables are to be validated.
#' @param .progress_cat Should the progress of the function be displayed on the console? If \code{TRUE} the name of the element about to be validated is shown.
#' @param char Return as character vector, in particular marking -- any complete or completely absent values.
#' @return A named logical vector, whether or not the variable complies with the style requirements. A value of \code{NA} indicates the variable
#' was not checked (perhaps because it is absent from \code{heims_data_dict}).
#' @details For early detection of invalid results, the type of the variable (in particular integer vs double) is considered first,
#' vetoing a \code{TRUE} result if different.
#' @examples
#' X <- data.frame(E306 = c(0, 1011, 999, 9998))
#' validate_elements(X)  # FALSE
#' prop_elements_valid(X)
#' X <- data.frame(E306 = as.integer(c(0, 1011, 999, 9998)))
#' validate_elements(X)  # TRUE
#'
#' @importFrom magrittr %>%
#' @importFrom hutils if_else

#' @rdname element_validation
#' @export validate_elements
validate_elements <- function(DT, .progress_cat = FALSE){
  out <- rep_len(NA, ncol(DT))

  avbl_noms <- names(heims_data_dict)[names(heims_data_dict) %in% names(DT)]

  # These suffixes define the insert method/event, not the variable
  # _A ==> initial, unless present in data dictionary
  noms <-
    if_else(names(DT) %in% avbl_noms,
            names(DT),
            gsub("A$", "", gsub("_[12]", "", names(DT))))

  # e550 == E550
  noms <- gsub("^e([0-9]+)$", "E\\1", noms)

  for (n in seq_along(DT)){
    nom <- noms[n]
    if (.progress_cat){
      cat(nom, ".", sep = "")
    }
    if (!is.null(heims_data_dict[[nom]]) && is.function(heims_data_dict[[nom]]$validate)){
      DTn <- DT[[n]]
      out[n] <- heims_data_dict[[nom]]$validate(DTn[!is.na(DTn)])
    }
  }
  if (.progress_cat){
    cat("\n")
  }
  names(out) <- names(DT)
  out
}

#' @rdname element_validation
#' @export prop_elements_valid
prop_elements_valid <- function(DT, char = FALSE){
  out <- rep_len(NA_real_, ncol(DT))

  noms <- gsub("A$", "", gsub("_[12]", "", names(DT)))

  # e550 == E550
  noms <- gsub("^e([0-9]+)$", "E\\1", noms)

  for (n in seq_along(DT)){
    nom <- noms[n]
    if (!is.null(heims_data_dict[[nom]]) && is.function(heims_data_dict[[nom]]$validate)){
      DTn <- DT[[n]]

      if (is.function(heims_data_dict[[nom]]$ad_hoc_prepare)){
        DTn <- heims_data_dict[[nom]]$ad_hoc_prepare(DTn)
      }

      DTn <- DTn[!is.na(DTn)]

      if (heims_data_dict[[nom]]$validate(DTn)){
        out[n] <- if (char) "--" else 1
      } else {
        if (!is.null(heims_data_dict[[nom]]) && is.function(heims_data_dict[[nom]]$valid)){
          prop <- mean(heims_data_dict[[nom]]$valid(DTn), na.rm = TRUE)
          out[n] <- if (char) paste0(round(prop * 100), "%") else prop
        }
      }
    }
  }
  names(out) <- names(DT)
  out
}

#' @rdname element_validation
#' @export count_elements_invalid
count_elements_invalid <- function(DT, char = FALSE){
  out <- rep_len(NA_real_, ncol(DT))

  noms <- gsub("A$", "", gsub("_[12]", "", names(DT)))

  # e550 == E550
  noms <- gsub("^e([0-9]+)$", "E\\1", noms)

  for (n in seq_along(DT)) {
    nom <- noms[n]
    if (!is.null(heims_data_dict[[nom]]) && is.function(heims_data_dict[[nom]]$validate)) {
      DTn <- DT[[n]]

      if (is.function(heims_data_dict[[nom]]$ad_hoc_prepare)) {
        DTn <- heims_data_dict[[nom]]$ad_hoc_prepare(DTn)
      }

      DTn <- DTn[!is.na(DTn)]

      if (heims_data_dict[[nom]]$validate(DTn)) {
        out[n] <- if (char) "--" else 0L
      } else {
        if (AND(!is.null(heims_data_dict[[nom]]),
                # Ensure 'validate' does not get mistaken for valid
                # due partial string matching.
                AND("valid" %in% names(heims_data_dict[[nom]]),
                    is.function(heims_data_dict[[nom]]$valid)))) {
          prop <- sum(!heims_data_dict[[nom]]$valid(DTn), na.rm = TRUE)
          out[n] <- if (char) paste0(round(prop * 100), "%") else prop
        }
      }
    }
  }
  names(out) <- names(DT)
  out
}
