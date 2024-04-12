
test_that("sum of each row in M0 corresponds to the corresponding r0", {
  group_n <- 5
  size <- c(10, 20, 30, 40, 50)
  r0 <- c(1, 2, 3, 4, 5)
  gamma <- c(1, 2, 0.5, 5, 0.1)
  name <- LETTERS[1:group_n]

  M0 <- generate_M0(
    group_n = group_n,
    name = name,
    size = size,
    r0 = r0,
    gamma = gamma
  )
  expect_equal(
    as.numeric(rowSums(M0)),
    r0
  )
})
