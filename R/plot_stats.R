#' Plot incidence and proportion of susceptible over time
#'
#' This function generates two plots: incidence and proportion of susceptible over time.
#'
#' @param stats A dataframe returned by simulate_groups()$stats
#'
#' @return A list containing two ggplots: incidence and proportion of susceptible over time.
#'
#' @export

plot_stats <- function(stats) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
  # Incidence
  incidence <-
    ggplot2::ggplot(data = stats,
                    ggplot2::aes(x = time, y = new_cases, fill = group)) +
    ggplot2::geom_col(position = "stack", col = "black") +
    ggplot2::labs(x = "Day", y = "Cases", fill = "Group") +
    ggplot2::ggtitle("Incidence over time") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  # Proportion of Susceptible
  prop_susceptibles <-
    ggplot2::ggplot(data = stats,
                    ggplot2::aes(x = time, y = prop_susceptible, col = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.1),
      limits = c(0, NA),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = "Day", y = "Proportion of susceptible", col = "Group") +
    ggplot2::ggtitle("Proportion of susceptible over time") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(list(incidence,
              prop_susceptibles))
  } else {
    stop("Please install ggplot2 to use this function")
  }
}
