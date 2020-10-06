assert_not_empty = function(df) {
    stopifnot(is.data.frame(df))
    stopifnot(nrow(df) > 0)
}
