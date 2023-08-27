#' Get Proportion of Susceptibles within a Time Window
#'
#' Calculates the proportion of susceptibles in each group within a specified time window.
#'
#' @param data A linelist.
#' @param t The time point (inclusive).
#' @param n_groups The total number of groups.
#' @param size A numeric vector of length \code{n_groups} containing the initial known sizes of the groups.
#' @param name A character vector of length \code{n_groups} containing the names of the groups.
#'
#' @return A numeric vector with the calculated proportions of susceptibles in each group.
#' @examples
#' data <- data.frame(
#'   group = c("A", "B", "B", "A"),
#'   id = c("HmPsw2", "WtYSxS", "2Kxtgd", "xtgn1T"),
#'   source = c(NA, NA, "WtYSxS", "WtYSxS"),
#'   source_group = c(NA, NA, "B", "B"),
#'   date_onset = c(0, 0, 2, 3)
#' )
#'
#' n_groups <- 2
#' size <- c(10, 15)
#' name <- c("A", "B")
#' t <- 2
#'
#' get_prop_susceptibles(data, t, n_groups, size, name)
#' @export


get_prop_susceptibles <-
  function(data, t, n_groups, size, name) {
    filtered_data <- subset(data, date_onset <= t)
    infected <- table(filtered_data$group)
    prop_infected <- as.vector(infected) / size
    prop_susceptible <- 1 - prop_infected

    return(prop_susceptible)
  }

