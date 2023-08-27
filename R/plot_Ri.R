#' Plot Ri Values
#'
#' This function calculates Ri values based on the provided data and name, and then plots the Ri values. It returns a combined plot with facet and group views.
#'
#' @param data A data frame containing the data for calculation.
#' @param name The name of the variable to be used for Ri calculation.
#' @param r0 The basic reproduction numbers to plot the hlines.
#' @return A combined plot of Ri values for facet and group views.
#' @export
plot_Ri <- function(data, name, r0) {
  dat <- o2groups::get_Ri(data)
  Ri_dat <- Ri_data(dat, name)
  p_Ri_facet <- plot_Ri_facet(Ri_dat$Ri_facet)
  p_Ri_group <- plot_Ri_group(Ri_dat$Ri_group, name = name, r0 = r0)

  p_Ri <- patchwork::wrap_plots(
    p_Ri_facet,
    p_Ri_group,
    ncol = 1,
    guides = "collect",
    heights = c(3, 1)
  )

  return(p_Ri)
}


#############
# Helpers
#############

#' Calculate Ri data for each date of onset
#'
#' This function calculates Ri values based on the provided data and name. It returns a list containing the Ri values for each facet and group.
#'
#' @param data A data frame containing the data for calculation.
#' @param name The name of the variable to be used for Ri calculation.
#'
#' @return A list with two data frames: Ri_facet and Ri_group.
#' @keywords internal
Ri_data <- function(data, name) {
  Ri_facet <- suppressMessages({
    data %>% select(group, all_of(name), date_onset) %>%
      pivot_longer(cols = all_of(name),
                   names_to = "target") %>%
      group_by(group, target, date_onset) %>%
      summarise(Ri = mean(value))
  })

  Ri_group <- suppressMessages({
    data %>% select(group, Ri, date_onset) %>%
      group_by(group, date_onset) %>%
      summarise(Ri = mean(Ri))
  })

  return(list(Ri_facet = Ri_facet, Ri_group = Ri_group))
}


#' Plot Ri Values for Facet View
#'
#' This function plots Ri values for the facet view based on the provided Ri_facet data frame.
#'
#' @param Ri_facet A data frame containing Ri values for each facet and group.
#'
#' @return A plot of Ri values for the facet view.
#' @keywords internal
plot_Ri_facet <- function(Ri_facet) {
  p_Ri_facet <- Ri_facet %>%
    ggplot(aes(
      x = date_onset,
      y = Ri,
      col = group
    )) +
    facet_grid(target ~ group,  scales = "free_y") +
    geom_point() +
    geom_line() +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none"
    )

  return(p_Ri_facet)
}

#' Plot Ri Values for Group View
#'
#' This function plots Ri values for the group view based on the provided Ri_group data frame.
#'
#' @param Ri_group A data frame containing Ri values for each group.
#' @param name The name of the variable to be used for Ri calculation.
#' @param r0 The basic reproduction numbers to plot the hlines.
#'
#' @return A plot of Ri values for the group view.
#' @keywords internal
plot_Ri_group <- function(Ri_group, name = name, r0 = r0) {
  p_Ri_group <- Ri_group %>%
    ggplot(aes(
      x = date_onset,
      y = Ri,
      col = group
    )) +
    facet_grid(~ group, scales = "free_y") +
    geom_point() +
    geom_line() +
    geom_hline(data = tibble(group = name, r0 = r0),
               aes(yintercept = r0, col = group, group = group),
               lty = "dotted") +
    theme_bw() +
    theme(strip.text.x = element_blank(),
          legend.position = "none") +
    labs(y = "")

  return(p_Ri_group)
}

