# Run a simulation and try to retrieve delta from early stage mixing
#
# test_that("Retrieve Delta with Mcol_to_delta", {
#
#   duration <- 100
#   n_groups <- 3
#   size <- c(1000, 500, 1000)
#   name <- LETTERS[1:3]
#   delta <- c(1,3,10)
#   intro_group <- "A"
#   intro_n <- 10
#   r0 <- c(4,2,4)
#   generation_time <- c(0, 0.1, 0.3, 0.4, 0.2)
#   n_cores <- parallel::detectCores() - 2
#   n_simulations <- 50
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
#   data = out$data
#   stats = out$stats
#
#
#   t <- sapply(name, function(x) {
#     df <- subset(stats, group == x)
#     t <- df$time[which.max(df$prop_susceptible<=0.9)]
#     return(t)
#   })
#
#   Mcol <- o2groups::get_mixing(data, max_t = round(min(t)))$Mcol
#
#   output_delta <- o2groups::Mcol_to_delta(
#     n_groups = n_groups,
#     size = size,
#     name = name,
#     Mcol = Mcol
#   )
#
#   # Compare the actual and expected outputs
#   for (i in 1:n_groups) {
#     diff <- abs(delta[i] - output_delta[[i]])
#     expect_true(diff <= 0.8,
#                 sprintf("Absolute difference between input delta[%s] and Mcol_to_delta()[[%s]] is greater than 0.5", i, i))
#   }
#
# })


