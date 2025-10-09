#' Backtrack Items Helper Function
#' Internal function to backtrack through the dynamic programming matrix
#' to find which items were selected in the optimal solution.
#'
#' @param m A matrix containing the dynamic programming solution values.
#' @param x A data.frame with two numeric columns: \code{v} for item values and \code{w} for item weights.
#'
#' @return An integer vector of indices corresponding to the selected items.
backtrack_items <- function(m, x) {
  n <- nrow(x)
  W <- ncol(m) - 1
  w <- c(0, as.integer(x$w))

  items <- integer(0)
  i <- n + 1
  j <- W + 1

  while (i > 1 && j > 1) {
    if (m[i, j] != m[i - 1, j]) {
      items <- c(i - 1, items)
      j <- j - w[i]
      i <- i - 1
    } else {
      i <- i - 1
    }
  }
  items
}

#' Dynamic Programming Knapsack Function
#' Solves the 0/1 knapsack problem using dynamic programming.
#' This algorithm finds the optimal solution by building a table of subproblems,
#' where each cell represents the maximum value achievable for a given capacity
#' and subset of items.
#'
#' @param x A data.frame with two numeric columns: \code{v} for item values and \code{w} for item weights.
#' @param W A positive numeric value representing the maximum capacity of the knapsack.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{value}{The maximum total value achievable within the knapsack capacity.}
#'   \item{elements}{An integer vector of indices corresponding to the selected items.}
#' }
#' @export
knapsack_dynamic <- function(x, W) {
  stopifnot(validate_knapsack_input(x, W))
  n <- nrow(x)

  values <- c(0, x$v)
  weights <- c(0, as.integer(x$w))

  m <- matrix(0, nrow = n + 1, ncol = W + 1)

  for (i in 2:(n + 1)) {
    for (j in 2:(W + 1)) {
      if (weights[i] >= j) {
        m[i, j] <- m[i - 1, j]
      } else {
        m[i, j] <- max(m[i - 1, j], m[i - 1, j - weights[i]] + values[i])
      }
    }
  }
  value <- m[n + 1, W + 1]
  elements <- backtrack_items(m = m, x = x)
  list(value = value, elements = elements)
}