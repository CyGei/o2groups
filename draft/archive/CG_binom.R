library(ggplot2)

# Create an empty data frame to store the results
results <- data.frame(time_window = character(),
                      group = character(),
                      estimate = numeric(),
                      lower_ci = numeric(),
                      upper_ci = numeric())

# Specify the size of the time window (in days)
window_size <- 2

# Get the unique timepoints
timepoints <- sort(unique(data$date_infection))

# Calculate Mt_to_delta_ci for each 5-day time window
for (i in 1:(length(timepoints) - window_size + 1)) {
  min_t <- timepoints[i]
  max_t <- timepoints[i + window_size - 1]

  delta_df <- as.data.frame(Mt_to_delta_ci(
    data,
    min_t = min_t,
    max_t = max_t,
    n_groups,
    size,
    name,
    r0
  ))

  # Create a data frame with the results for the current time window
  result_df <- data.frame(time_window = paste(min_t, max_t, sep = "-"),
                          t_start = min_t,
                          group = row.names(delta_df),
                          estimate = delta_df$est,
                          lower_ci = delta_df$low,
                          upper_ci = delta_df$high)

  # Append the results to the overall results data frame
  results <- rbind(results, result_df)
}

# Convert the time_window column to a factor for proper ordering in the plot
results$time_window <- factor(results$time_window, levels = unique(results$time_window))

# Plot the results using ggplot2
results %>%
  filter(t_start >= 10 & t_start<=20) %>%
  ggplot( aes(x = time_window, y = estimate, color = group)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_hline(data = tibble(group = LETTERS[1:n_groups],
                           delta = delta),
             aes(yintercept = delta, col = group)) +
  labs(x = "Time Window", y = "Delta Estimate", color = "Group") +
  theme_bw()








library(ggplot2)

# Create an empty data frame to store the results
results <- data.frame(timepoint = numeric(),
                      group = character(),
                      estimate = numeric(),
                      lower_ci = numeric(),
                      upper_ci = numeric())

# Iterate over each unique timepoint
for (tp in sort(unique(data$date_infection))) {
  tryCatch({
    delta <- as.data.frame(Mt_to_delta_ci(data,
                            min_t = tp,
                            max_t = tp,
                            n_groups,
                            size,
                            name,
                            r0))

    # Create a data frame with the results for the current timepoint
    result_df <- data.frame(timepoint = tp,
                            group = row.names(delta),
                            estimate = delta$est,
                            lower_ci = delta$low,
                            upper_ci = delta$high)

    # Append the results to the overall results data frame
    results <- rbind(results, result_df)

  }, error = function(e) {
    # Handle the error (e.g., print an error message)
    cat("Error occurred for timepoint", tp, ":", conditionMessage(e), "\n")

    # Create a data frame with NA values for the current timepoint
    result_df <- data.frame(timepoint = tp,
                            group = rep(NA, n_groups),
                            estimate = rep(NA, n_groups),
                            lower_ci = rep(NA, n_groups),
                            upper_ci = rep(NA, n_groups))

    # Append the empty results to the overall results data frame
    results <- rbind(results, result_df)
  })
}

# Plot the results using ggplot2
results %>%
  filter(estimate != Inf) %>%
  ggplot(aes(x = timepoint, y = estimate, color = group)) +
  geom_point() +
  labs(x = "Timepoint", y = "Delta Estimate", color = "Group") +
  theme_bw()
