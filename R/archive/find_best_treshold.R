#' Find Best Threshold for Delta Estimation
#'
#' This function finds the best time threshold to estimate the delta values for each group.
#'
#' @param delta Numeric vector representing the true delta values for each group.
#' @param data The input data frame containing the necessary columns.
#' @param n_groups Number of groups.
#' @param size An integer vector representing the sizes of each group.
#' @param name A character vector containing the names of each group (respective to the `size` vector).
#'
#' @return A data frame with the best time threshold, estimated delta, difference from true delta,
#' proportion of susceptibles, and group information for each group.
#'
#' @keywords internal

find_best_threshold <-
  function(delta, data, n_groups, size, name) {
    names(delta) <- name

    est <- purrr::map(
      .x =
        min(data$date_onset[!is.na(data$source)]):max(data$date_onset[!is.na(data$source)]),
      ~ early_delta(
        data = data,
        min_t = 0,
        max_t = .x,
        size = size,
        name = name
      ) %>%
        dplyr::as_tibble(rownames = "group") %>%
        dplyr::mutate(t = .x)
    ) %>%  dplyr::bind_rows()

    diff <-
      dplyr::left_join(est,
                       tibble::tibble(true_delta = delta, group = name),
                       by = "group") %>%
      dplyr::mutate(diff = round(abs(est - true_delta), digits = 3))


    sus <- purrr::map(
      .x =
        min(data$date_onset[!is.na(data$source)]):max(data$date_onset[!is.na(data$source)]),
      ~ get_prop_susceptibles(
        data = data,
        t = .x,
        size = size,
        name = name
      ) %>%
        dplyr::as_tibble(group = name) %>%
        dplyr::mutate(group = name,
                      t = .x) %>%
        dplyr::rename(prop_sus = value)
    ) %>%  dplyr::bind_rows()

    out <-
      dplyr::left_join(diff, sus, by = c("group", "t"))

    return(out)

  }

