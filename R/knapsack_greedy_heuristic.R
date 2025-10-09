greedy_knapsack <- function(x, W) {
  stopifnot(validate_knapsack_input(x, W))

  sorted <- x[order((x$v / x$w), decreasing = TRUE),]
  s1 <- c()
  s2 <- 0
  for (k in seq_len(length(sorted))) {
    if (sum(s1) + sorted$v[k] <= W) {
      s1 <- c(s1, sorted$v[k])
    } else {
      s2 <- sorted[k + 1]
      break
    }
  }
  if (sum(s1) > s2) {
    s1
  } else {
    s2
  }
}