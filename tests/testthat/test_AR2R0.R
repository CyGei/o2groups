library(epitrix)
test_that("R0 from simulations is not statistically different from true R0", {
  true_R0 <- 3
  set.seed(1)
  R0 <- sapply(1:50, function(i){
    duration <- 100
    group_n <- 1
    size <- 1000
    name <- "A"
    gamma <- 1
    intro_n <- 10
    r0 <- true_R0
    generation_time <- c(0, 0.1, 0.3, 0.4, 0.2)
    generation_time <- generation_time / sum(generation_time)
    incubation_period <- NULL
    dt <- 1
    out <- simulate_groups(
      duration = duration,
      group_n = group_n,
      size = size,
      name = name,
      gamma = gamma,
      intro_n = intro_n,
      r0 = r0,
      generation_time = generation_time,
      incubation_period = incubation_period,
      dt = dt
    )
    AR <- nrow(out) / size
    return(epitrix::AR2R0(AR))
  })
  pval <- t.test(R0, mu = true_R0)$p.value
  expect_true(pval > 0.05)
})

