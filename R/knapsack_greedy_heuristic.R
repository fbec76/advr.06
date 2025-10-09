#' Greedy Heuristic Knapsack Function
#' Solves the knapsack problem using a greedy heuristic approach.
#' This algorithm sorts items by their value-to-weight ratio in descending order
#' and greedily selects items until the knapsack capacity is reached.
#' Note: This is a heuristic method that may not always find the optimal solution.
#'
#' @param x A data.frame with two numeric columns: \code{v} for item values and \code{w} for item weights.
#' @param W A positive numeric value representing the maximum capacity of the knapsack.
#' @param fast A logical value. If TRUE, uses the C++ implementation for better performance. Default is FALSE.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{value}{The total value of the selected items (may not be optimal).}
#'   \item{elements}{An integer vector of indices corresponding to the selected items.}
#' }
#'
#' @export
greedy_knapsack <- function(x, W, fast = FALSE) {
  stopifnot(validate_knapsack_input(x, W))

  if (fast) {
    return(greedy_knapsack_cpp(x, W))
  }

  sorted <- x[order((x$v / x$w), decreasing = TRUE),]
  s1 <- sorted[0,]
  s2 <- sorted[0,]
  for (k in seq_len(nrow(sorted))) {
    if ((sum(s1$w) + sorted$w[k]) <= W) {
      s1 <- rbind(s1, sorted[k,])
    } else {
      s2 <- sorted[which.max(sorted$v[sorted$w <= W]), , drop = FALSE]
      break
    }
  }
  value <- s2$v
  elements <- s2
  if (sum(s1$v) > s2$v[[1]]) {
    value <- sum(s1$v)
    elements <- s1
  }
  list("value" = value,
       "elements" = unlist(as.numeric(rownames(elements)), use.names = FALSE))
}