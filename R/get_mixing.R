#' Get mixing proportions between groups
#'
#' This function the data from \code{simulate_groups} to calculate the proportions of infections between groups.
#'
#' @param data A data frame containing columns for the source group \code{source_group}, infected group \code{group}, and date of infection \code{date_infection}.
#' @param min_t The minimum date to consider when calculating mixing proportions. Defaults to the earliest date in the \code{date_infection} column.
#' @param max_t The maximum date to consider when calculating mixing proportions. Defaults to the latest date in the \code{date_infection} column.
#' @return A list with two elements: \code{Mcol}, a matrix of mixing proportions where the columns represent the source group and the rows represent the infected group; and \code{result}, a data frame with columns for the source group, infected group, the number of infections from the source to infected group, and the proportion of infections from the source to infected group.
#' @export
#' @examples
#'out <-
#'simulate_groups(
#'  duration = 100,
#'  n_groups = 2,
#'  size = c(10, 30),
#'  name = c("A", "B"),
#'  delta = c(5, 2),
#'  intro_n = c(1, 2),
#'  r0 = c(4, 3),
#'  generation_time = simulacr::make_disc_gamma(mean = 5, sd = 2)$d(1:30)
#')$data
#'incubation_period = simulacr::make_disc_gamma(mean = 3, sd = 1)
#'out["date_onset"] <- out["date_infection"] + incubation_period$r(nrow(out))
#'get_mixing(out, min_t = 0,  max_t = 10)



get_mixing <-
  function(data,
           min_t = min(data$date_onset),
           max_t = max(data$date_onset)
           ) {

    df <-
      subset(
        data,
        date_onset >= min_t &
          date_onset <= max_t &
          !is.na(source),
        select = c(source_group, group)
      )
    # df <-
    #   aggregate(df,
    #             by = list(from = df$source_group, to = df$group),
    #             FUN = length)

    counts <- t(table(df$source_group, df$group))
    freqs <- prop.table(counts, margin = 2)

    result <-
      expand.grid(source_group = rownames(freqs), group = colnames(freqs))
    result$n <-  as.vector(t(counts))
    result$freq = as.vector(t(freqs))

    return(list(Mcol = freqs,
                result = result))
  }
