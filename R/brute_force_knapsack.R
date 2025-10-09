RNGversion(min(as.character(getRversion()), "3.5.3"))
suppressWarnings(set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion"))

n <- 2000
knapsack_objects <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

brute_force_knapsack <- function(x, W) {
  stopifnot(validate_knapsack_input(x, W))

  n <- nrow(x)
  best_value <- 0
  best_combination <- integer(0)

  for (i in 1:(2^n - 1)) {
    combo <- as.integer(intToBits(i))[1:n]
    total_weight <- sum(combo * x$w)
    total_value <- sum(combo * x$v)


    if (total_weight <= W && round(total_value) > round(best_value)) {
      best_value <- total_value
      best_combination <- which(combo == 1)
    }
  }

  return(list(value = round(best_value), elements = best_combination))
}

