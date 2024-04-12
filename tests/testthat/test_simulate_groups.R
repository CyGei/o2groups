for (i  in 1:50) {
  test_that("simulate_groups works with random parameters", {
    # Generate random parameters
    duration <- sample(1:150, 1)
    group_n <- sample(1:10, 1)
    size <- sample(1:200, group_n, replace = TRUE)
    name <- sample(LETTERS, group_n)
    gamma <- runif(group_n, 0, 1e4)
    intro_n <- sapply(size, function(x)
      sample(0:x, 1))
    if (all(intro_n == 0)) {
      intro_n[sample(1:group_n, 1)] <- 1
    }
    r0 <- runif(group_n, 0.5, 5)
    generation_time <- dpois(0:50, lambda=runif(1, 1, 5))
    incubation_period <- NULL

    result <- simulate_groups(
      duration = duration,
      group_n = group_n,
      size = size,
      name = name,
      gamma = gamma,
      intro_n = intro_n,
      r0 = r0,
      generation_time = generation_time
    )

    # Check that the result is a data frame
    expect_is(result, "data.frame")

    # Check that the result has the correct columns
    expect_equal(colnames(result),
                 c("group", "id", "source", "source_group", "date_infection"))
  })
}
