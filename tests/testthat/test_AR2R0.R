#
# # Single Group Test:
# library(epitrix)
#
# test_that("epitrix::AR2R0 returns value close to input r0", {
#   n_simulations <- 20
#   n_cores <- 4
#   duration <- 100
#   n_groups <- 1
#   size <- 1000
#   name <- "A"
#   delta <- 1
#   intro_group <- "A"
#   intro_n <- 10
#   r0 <- 3
#   generation_time <- c(0, 0.1, 0.3, 0.4, 0.2)
#
#   out <- o2groups::simulate_groups_parallel(
#     n_simulations = n_simulations,
#     n_cores = n_cores,
#     duration = duration,
#     n_groups = n_groups,
#     size = size,
#     name = name,
#     delta = delta,
#     intro_group = intro_group,
#     intro_n = intro_n,
#     r0 = r0,
#     generation_time = generation_time
#   )
#
#   stats <- out$stats
#
#   # Filter data by the max time
#   filtered <- stats[stats$time == max(stats$time),]
#   # Group by simulation and calculate attack rate
#   grouped <- aggregate(filtered$n_infected, by=list(filtered$simulation), FUN=sum)
#   grouped$attack_rate <- grouped$x / filtered$size
#   # Calculate the mean of attack rates
#   mean_attack_rate <- mean(grouped$attack_rate)
#   # Convert attack rate to R0
#   ar2r0 <- epitrix::AR2R0(mean_attack_rate)
#
#   expect_true(abs(ar2r0 - r0) < 0.5,
#               "epitrix::AR2R0 is not within 0.5 of the input r0 value")
# })
#
