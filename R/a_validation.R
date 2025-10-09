validate_knapsack_input <- function(x, W) {
  is.data.frame(x) &&
    length(setdiff(c("v", "w"), colnames(x))) == 0 &&
    is.numeric(x$v) &&
    is.numeric(x$w) &&
    all(x$v >= 0) &&
    all(x$w >= 0) &&
    is.numeric(W) &&
    is.atomic(W) &&
    length(W) == 1
}
