#' Utility functions
#' @description Only included here because of the unusual nature of \code{\link{heims_data_dict}}.
#' @name utilities
#' @param v A vector.

#' @rdname utilities
#' @export AND
AND <- `&&`

#' @rdname utilities
#' @export OR
OR <- `||`

#' @rdname utilities
#' @export never
never  <- function(v) logical(length(v))

#' @rdname utilities
#' @export every
every <- function(v) !never(v)

#' @rdname utilities
#' @export always
always <- function(v) TRUE

#' @rdname utilities
#' @export is.Date
is.Date <- function(v){
  if (is.numeric(v)) {
    and(between(v / 10000, 1899, 2017),
        and(between((v %% 10000L) %/% 100, 1, 12),
            between(v %% 100, 1, 31)))
  } else {
    !is.na(as.Date(v, format = "%Y%m%d"))
  }
}

#' @rdname utilities
#' @export is.YearMonth
is.YearMonth <- function(v) is.Date(v * 100L + 1L)


#' @rdname utilities
#' @param x,n vectors
#' @export nth_digit_of
#' @details \code{nth_digit_of} returns the nth digit of the number \strong{starting from the units and going up in magnitude.}
#' @examples nth_digit_of(503, 1) == 1
nth_digit_of <- function(x, n){
  (x %% 10^n) %/% 10^(n - 1)
}

#' @rdname utilities
#' @param ... Passed to other functions
#' @export between
between <- function(...) data.table::between(...)

#' @rdname utilities
#' @export or
or <- function(...) magrittr::or(...)

#' @rdname utilities
#' @export and
and <- function(...) magrittr::and(...)

#' @rdname utilities
#' @export if_else
if_else <- function(...) hutils::if_else(...)

#' @rdname utilities
#' @export coalesce
coalesce <- function(...) hutils::coalesce(...)

#' @rdname utilities
#' @export %fin%
#' @param a Element suspected to be in \code{tbl}
#' @param tbl A lookup table.
#' @importFrom fastmatch fmatch
`%fin%` <- function(a, tbl) fmatch(a, tbl, 0L, NULL) > 0L

`%notin%` <- Negate("%in%")

#' @rdname utilities
#' @export rm_leading_0s
rm_leading_0s <- function(v){
  if (is.character(v)) {
    v <- gsub("^0+", "", v, perl = TRUE)
  }
  v
}

#' @rdname utilities
#' @export as.integer64
# The importFrom is what makes bit64 load with heims, which is what registers
# its S3 methods. Without it, subsetting an integer64 column silently drops
# through to the default `[` and returns a bare double.
#' @importFrom bit64 integer64
as.integer64 <- function(v) bit64::as.integer64(v)

#' @rdname utilities
#' @export is.integer64
is.integer64 <- function(v) bit64::is.integer64(v)

#' @rdname utilities
#' @export force_integer
force_integer <- function(v){
  suppressWarnings(as.integer(v))
}

#' @rdname utilities
#' @export ymd
ymd <- function(...) lubridate::ymd(...)


