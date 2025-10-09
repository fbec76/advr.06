# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <- data.frame(w = sample(1:4000, size = n, replace = TRUE),
#                                v = runif(n = n, 0, 10000))

test_that("[dynamic] knapsack", {
  wiki_example <- data.frame("v" = c(5, 4, 3, 2), "w" = c(4, 3, 2, 1))
  res <- knapsack_dynamic(x = wiki_example, W = 6)
  expect_equal(res$value, 9)
  expect_equal(res$elements, c(4, 3, 2))
})