# library(EpiEstim)
#
# test_that("EpiEstim::estimate_R reaches Rt = 1 at the same time as Ri + mean generation time", {
#   duration <- 100
#   n_groups <- 1
#   size <- 1000
#   name <- "A"
#   delta <- 1
#   intro_group <- "A"
#   intro_n <- 5
#   r0 <- 3
#   generation_time <- c(0, 0.1, 0.3, 0.4, 0.2)
#
#   out <- o2groups::simulate_groups(
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
#   stats <- out$stats
#   data <- out$data
#
#   # Epidemic End:
#   end_index <- max(which(stats$new_cases != 0))
#
#   #Epiestim:
#   Restim <- EpiEstim::estimate_R(stats$new_cases[1:end_index+1],
#                        method="non_parametric_si",
#                        config =  EpiEstim::make_config(list(
#                          si_distr = generation_time)))
#
#   # Find when Rt reaches threshold of <= 1
#   subset_Restim <- subset(Restim$R, `Mean(R)` < 1)
#   threshold_Rt <- min(subset_Restim$t_end)
#
#   # Find when Ri reaches threshold of <= 1
#   Ri <- o2groups::get_Ri(data)
#   mean_GT <- sum(generation_time * seq_along(generation_time))
#   Ri <- aggregate(Ri ~ group + date_infection, data = Ri, FUN = mean)
#   threshold_Ri <- min(which(Ri$Ri <= 1)) + 1
#   #Add mean GT
#   threshold_Ri <- threshold_Ri + mean_GT
#
#   # Find the day of the 75th percentile of generation_time
#   GT_75th <- which(cumsum(generation_time) >= 0.75)[1]
#
#   expect_true(abs(threshold_Ri - threshold_Rt) < GT_75th,
#               "Ri & Rt <= 1 not at the same time (controlling for GT)")
# })
