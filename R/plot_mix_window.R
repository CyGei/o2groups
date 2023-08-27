#' Plot Mixing Windows
#'
#' This function calculates the mixing window data based on the provided data and window length, and then plots the mixing windows. It returns a plot showing the mixing windows for each group and source group, along with confidence intervals and a dotted line representing the ground truth values.
#'
#' @param data A data frame containing the data for calculation.
#' @param window_length The length of each mixing window.
#'
#' @return A plot showing the mixing windows with confidence intervals and ground truth values.
#' @keywords internal
plot_mix_window <- function(data, window_length, n_groups, size, name, delta) {
  mix_window_data <- mix_window_data(data, window_length)

  Truth <- generate_Mcol(
    n_groups = n_groups,
    size = size,
    name = name,
    delta = delta
  ) %>%
    as.data.frame(.) %>%
    rownames_to_column(var = "group") %>%
    pivot_longer(-group, names_to = "source_group", values_to = "value") %>%
    select(source_group, group, value) %>%
    arrange(source_group)

  p_mix <- ggplot() +
    aes(col = source_group) +
    facet_grid(group ~ source_group) +
    geom_segment(data = mix_window_data,
                 aes(
                   x = t_start,
                   xend = t_end,
                   y = freq ,
                   yend = freq
                 )) +
    geom_rect(
      data = mix_window_data,
      aes(
        ymin = lower_ci,
        ymax = upper_ci,
        xmin = t_start,
        xmax = t_end,
        fill = source_group
      ),
      alpha = 0.2,
      color = "white"
    ) +
    geom_label(data = mix_window_data,
               aes(
                 x = (t_start + t_end) / 2,
                 y = freq,
                 label = n,
                 group = window
               ),
               size = 3) +
    geom_hline(data = Truth,
               aes(yintercept = value), lty = "dotted") +
    theme_bw() +
    theme(legend.position = "none") +
    gg_col

  return(p_mix)
}


#' Calculate Mixing Window Data
#'
#' This function calculates the mixing window data based on the provided data and window length. It returns a data frame containing the mixing window information, including frequencies, confidence intervals, and window boundaries.
#'
#' @param data A data frame containing the data for calculation.
#' @param window_length The length of each mixing window.
#'
#' @return A data frame containing the mixing window information.
#' @keywords internal
mix_window_data <- function(data, window_length) {
  mix_window <- map(.x = seq(0, 100, window_length),
                    ~ {
                      get_mixing(data, min_t = .x, max_t = .x + window_length)$result %>%
                        mutate(t_start = .x, t_end = .x + window_length)
                    }) %>%
    bind_rows(., .id = "window") %>%
    mutate(window = as.integer(window)) %>%
    arrange(window) %>%
    group_by(window, source_group) %>%
    mutate(total_n = sum(n)) %>%
    rowwise() %>%
    mutate(
      est = binom.test(n, total_n)$estimate[[1]],
      lower_ci = binom.test(n, total_n)$conf.int[1],
      upper_ci = binom.test(n, total_n)$conf.int[2]
    )

  return(mix_window)
}

