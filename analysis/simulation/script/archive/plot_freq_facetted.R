plot_freq_faceted <- function(x,
                              y,
                              x_breaks = NULL,
                              y_breaks = NULL,
                              x_length_out = 10,
                              y_length_out = 10,
                              facet_var = NULL) {
  if (is.null(x_breaks)) {
    x_breaks <-
      seq(min(outcomes[[x]], na.rm = TRUE),
          max(outcomes[[x]], na.rm = TRUE),
          length.out = x_length_out
      )
  }
  if (is.null(y_breaks)) {
    y_breaks <-
      seq(min(outcomes[[y]], na.rm = TRUE),
          max(outcomes[[y]], na.rm = TRUE),
          length.out = y_length_out
      )
  }

  data <- outcomes %>%
    select(x, y, !!enquo(facet_var)) %>%
    mutate(
      x_bin = cut(.data[[x]], breaks = x_breaks),
      y_bin = cut(.data[[y]], breaks = y_breaks)
    ) %>%
    drop_na() %>%
    count(x_bin, y_bin, !!!syms(facet_var)) %>%
    group_by(x_bin, !!!syms(facet_var)) %>%
    mutate(
      denom = sum(n),
      freq = n / denom
    )

  plot <- ggplot(data, aes(x = x_bin, y = y_bin)) +
    facet_wrap(vars(!!!syms(facet_var))) +
    geom_tile(aes(fill = freq), col = "white") +
    geom_label(
      aes(x = x_bin, y = -1, label = scales::comma_format()(denom)),
      vjust = -0.5
    ) +
    scale_fill_viridis_c() +
    labs(x = paste(x, "Bin"), y = paste(y, "Bin"), fill = "Frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(plot)
}
plot_freq_faceted("size", "bias",
          facet_var = c("peak_coeff"))

