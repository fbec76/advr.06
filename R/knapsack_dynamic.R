backtrack_items <- function(i, j, m, w) {
  if (i > 1) {
    if (m[i, j] > m[i - 1, j]) {
      c(i - 1, backtrack_items(i - 1, j - w[i], m, w))
    } else {
      backtrack_items(i - 1, j, m, w)
    }
  }
}


#' @export
knapsack_dynamic <- function(x, W) {
  stopifnot(validate_knapsack_input(x, W))
  n <- nrow(unique(x)) + 1
  values <- c(0, x$v)
  weights <- c(0, x$w)

  m <- matrix(0, nrow = n, ncol = W + 1)

  for (i in 2:n) {
    for (j in 2:(W + 1)) {
      if (weights[i] > j) {
        m[i, j] <- m[i - 1, j]
      } else {
        m[i, j] <- max(m[i - 1, j], m[i - 1, j - weights[i]] + values[i])
      }
    }
  }
  value <- m[nrow(m), ncol(m)]
  elements <- backtrack_items(n, (W + 1), m = m, w = weights)
  list("value" = value, "elements" = unlist(elements, use.names = FALSE))
}