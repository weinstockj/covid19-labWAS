assert_not_empty = function(df) {
    stopifnot(is.data.frame(df))
    stopifnot(nrow(df) > 0)
}

inverse_normalize = function(x) {
    qnorm(rank(x, na.last = "keep") / (sum(!is.na(x)) + 1))
}

#' Test If A Variable Is An Integer
#' see ?is.integer
#'@export
is.wholenumber = function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
