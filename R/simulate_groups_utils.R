# get_stats -----------------------------------------------------------------------------------

#' Calculate statistics for each group over time
#'
#' This function calculates statistics for each group over time, including the number of susceptible and infected individuals, force of infection (FOI), infection rate, infection probability, and new cases.
#' @param data A dataframe from simulate_groups().
#' @param t The time point.
#' @param n_groups The number of groups.
#' @param name A character vector of length n_groups specifying the names of the groups.
#' @param size A numeric vector of length n_groups specifying the size of each group.
#' @param groupFOI A numeric vector of length n_groups specifying the FOI that each group receives at time t.
#' @param dt A numeric value specifying the time interval between timesteps.
#' @return A data frame containing the calculated statistics for each group at each time point.
#'
#' @keywords internal


get_stats <- function(data , t, n_groups, name, size, groupFOI, dt){

  stats <- data.frame(time = t,
                      group = name,
                      size = size)

#  Calculate the number of infected individuals for each group
  stats$n_infected <- sapply(name, function(group) {
    sum(data$group == group)
  })
  stats$n_susceptible <- size - stats$n_infected
  stats$prop_susceptible <- stats$n_susceptible / size
  stats$foi <- groupFOI
  stats$infection_rate <- stats$foi / size
  stats$infection_prob <- 1 - exp(-stats$infection_rate * dt)
  stats$new_cases <- stats::rbinom(
    n = n_groups,
    size = stats$n_susceptible,
    prob = stats$infection_prob
  )

  return(stats)

}


# Adds x/y coords to individuals based on their groups
#1. K-means clustering with K = n_groups.
#2. Use the Kmeans as the center points for each group.
#3. Generate the individuals coords using rnorm(mean = Kmeans).
#This modification uses the kmeans function with a large number of iterations to generate the means of the x and y coordinates for each group.
#This way, it is less likely that the clusters will overlap or be too close to one another.



# generate_clusters ---------------------------------------------------------------------------

#' Generate clustered data
#'
#' Adds x/y coords to individuals based on their groups
#' using K-means clustering to generate group means.
#'
#' @param n_groups the number of groups to generate.
#' @param size the number of individuals per group to generate. Default is 10 per group.
#' @param name the names of the groups. Default is "Group 1", "Group 2", ..., "Group n_groups".
#' @param spread the standard deviation of the x and y coordinates around the group means. Default is 0.5.
#' @param shape the shape of the clusters.Can be "random", "square", or "round". Default is "random".
#' @keywords internal

generate_clusters <-
  function(n_groups = NULL,
           size = NULL,
           name = NULL,
           spread = 0.5,
           shape = "random") {
    if (is.null(n_groups)) {
      num_groups <- length(unique(name))
    }
    if (is.null(size)) {
      size <- rep(10, n_groups)
    }
    if (is.null(name)) {
      name <- paste0("Group ", 1:n_groups)
    }

    # Generate the means of the x and y coordinates for each group.
    #kmeans needs a large number of data points to generate the means.
    data <-
      data.frame(x = stats::rnorm(n_groups * 10, 0, 1),
                 y = stats::rnorm(n_groups * 10, 0, 1))
    # nstart: algorithm will be run with 100 different centroid seeds.
    kmeans_result <-
      stats::kmeans(data, n_groups, nstart = 100, iter.max = 1000)
    # center points of each cluster.
    means_x <- kmeans_result$centers[, 1]
    means_y <- kmeans_result$centers[, 2]

    xy_coords <- data.frame(x = numeric(0), y = numeric(0))

    for (i in 1:n_groups) {
      if (shape == "square") {
        xy_coords_temp <-
          data.frame(
            x = stats::runif(size[i], means_x[i] - spread, means_x[i] + spread),
            y = stats::runif(size[i], means_y[i] -
                        spread, means_y[i] + spread)
          )
      } else if (shape == "round") {
        #generate a random radius and angle for each point.
        r <- stats::runif(size[i], 0, spread)
        theta <- stats::runif(size[i], 0, 2 * pi)
        #convert them to Cartesian coordinates.
        xy_coords_temp <- data.frame(x = means_x[i] + r * cos(theta),
                                     y = means_y[i] + r * sin(theta))
      } else {
        xy_coords_temp <-
          data.frame(
            x = rnorm(size[i], means_x[i], spread),
            y = rnorm(size[i], means_y[i], spread)
          )
      }
      xy_coords_temp$group <- name[i]
      xy_coords <- rbind(xy_coords, xy_coords_temp)
    }
    return(split(xy_coords, xy_coords$group))
  }



# sample_sources ------------------------------------------------------------------------------

#' Samples infectors based on their Force of Infection
#'
#' This functions samples infector IDs based on their FOI targeting the group of interest
#'
#' @param group The current group of interest
#' @param n_cases The number of new cases to be generated in that group at time t
#' @param data The data/linelist at time t
#' @param indivFOI The individual FOI for all potential infectors targeting the group of interest (will be used as weight for sampling)
#' @return A list of source IDs
#' @keywords internal

sample_sources <- function(group, n_cases, data, indivFOI) {

  # Infector IDs
  sources <- data$id

  # Get the foi values for the infectors in the group
  source_foi <- indivFOI[, group]

  # Normalize the foi values
  source_prob <- source_foi / sum(source_foi)

  # Sample the infectors with replacement based on their foi
  sampled_ids <- sample(sources, size = n_cases, prob = source_prob, replace = TRUE)
  # Return the sampled IDs
  return(sampled_ids)
}


# generate_new_cases --------------------------------------------------------------------------

#' Generate new cases at time t
#'
#' @param data The current linelist at time t
#' @param t The current iteration of the timestep
#' @param sources The `sources` object returned by sample_sources
#' @return A dataframe / linelist of new cases
#' @keywords internal

generate_new_cases <- function(data, t, sources) {

  # Get the number of new cases for each group
  n_new_cases <- lengths(sources)

  # Generate the IDs of new cases
  case_ids <- simulacr::draw_labels(sum(n_new_cases))

  # Get the source IDs/groups for the new cases
  source <- unlist(sources, use.names = FALSE)

  source_group <- data$group[match(source, data$id)]

  # Create a new dataframe with the new cases
  new_cases <- data.frame(
    #name of sources list refers to the destination group
    group = rep(names(sources), n_new_cases),
    id = case_ids,
    source = source,
    source_group = source_group,
    date_infection = rep(t, sum(n_new_cases)),
    row.names = NULL
    )

  return(new_cases)
}




# get_Ri --------------------------------------------------------------------------------------

#' Returns the Case Reproduction Number
#'
#' This function takes in a data frame with columns for source ID and group, and returns the data frame with the number of cases each ID generated in each group.
#'
#' @param data A data frame with columns for source ID and group.
#' @return The data frame with the number of cases each ID generated in each group.
#' @export
#' @examples
#' data <- data.frame(id = c(1, 2, 3, 4, 5),
#'                    source = c(NA, 1, 2, 2, 3),
#'                    group = c("A", "A", "B", "B", "A"))
#' get_Ri(data)

get_Ri <- function(data){

  #count the number of times someone appears as a source in each group
  Ri_mat <- with(data, table(source, group))

  # ID's total Ri
  Ri <- rowSums(Ri_mat)
  Ri_mat <- cbind(Ri_mat, Ri)


  Ri_df <- data.frame(id = row.names(Ri_mat),
                      Ri_mat,
                      row.names = NULL)

  #Merge back to data
  data <- merge(data, Ri_df, by = "id", all.x = TRUE)
  #data[names(Ri_df)[-1]] <- Ri_df[match(data$id, Ri_df$id), -1]

  # Where Ri is NA replace with 0
  data[names(Ri_df)[-1]][is.na(data[names(Ri_df)[-1]])] <- 0

  return(data)

}


# get_gt --------------------------------------------------------------------------------------


#' Compute generation time values
#'
#' Compute the generation time values for the transmission pairs.
#'
#' @param data The data element of `simulate_groups`.
#' @return The modified data frame with the added gt column.
#' @export
get_gt <- function(data) {
  data$gt <- data$date_infection - data$date_infection[match(data$source, data$id)]

}


# get_si --------------------------------------------------------------------------------------

#' Compute serial interval
#'
#' Compute the serial interval values for the transmission pairs.
#'
#' @param data The data element of `simulate_groups`. Please, name `date_onset` for the date of symptom onset column.
#' @return The modified data frame with the added si column.
#' @export
get_si <- function(data) {
  data$si <- data$date_onset - data$date_onset[match(data$source, data$id)]

}




# get_peak ------------------------------------------------------------------------------------

#' Get the estimated peak values for each group in the data.
#'
#' This function calculates the estimated peak values for each group in the provided data. The data should have columns named "group" and "date_onset".
#'
#' @param data A data frame containing the necessary columns "group" and "date_onset".
#' @return A named list of estimated peak values for each group.
#'
#' @importFrom incidence incidence estimate_peak
#' @keywords internal
#' @seealso [incidence::incidence()] [incidence::estimate_peak()]
get_peak <- function(data) {
  group_names <- unique(data$group)
  out <- vector("list", length(group_names))

  for (i in seq_along(group_names)) {
    group_data <- data[data$group == group_names[i], "date_onset"]
    incid <- incidence::incidence(dates = group_data)
    peak <- incidence::estimate_peak(incid)
    out[[i]] <- round(peak$estimated)
  }

  names(out) <- group_names
  return(out)
}


# Scale / Reverse - scale Delta ---------------------------------------------------
#' Scale raw delta values.
#'
#' This function takes a numeric value 'raw_delta' and scales it by
#' applying the formula: (raw_delta - 1) / (raw_delta + 1) for finite values,
#' and returns 1.0 for Infinite values.
#'
#' `raw_delta` is a non-negative real number constrained within the interval [0, ∞).
#' `raw_delta` < 1 indicates dissortativity.
#' `raw_delta` > 1 indicates assortativity.
#' `raw_delta` = 1 indicates neutrality.
#'
#' `scaled_delta` is a real number constrained within the interval [-1, 1].
#' `scaled_delta` < 0 indicates dissortativity.
#' `scaled_delta` > 0 indicates assortativity.
#' `scaled_delta` = 0 indicates neutrality.
#'
#' @param raw_delta A numeric value to be scaled.
#' @return The scaled delta value.
#' @export
scale <- function(raw_delta) {
  if (is.infinite(raw_delta)) {
    return(1)
  } else if (is.na(raw_delta)) {
    return(NA)
  } else if (is.numeric(raw_delta) && is.finite(raw_delta)) {
    scaled_delta <- (raw_delta - 1) / (raw_delta + 1)
    return(scaled_delta)
  } else {
    # Handle other cases, e.g., non-numeric values
    return(raw_delta)
  }
}



#' Reverse the standardisation of delta.
#'
#' This function takes a scaled delta value 'scaled_delta' and reverses
#' the standardisation by applying the formula: (1 + scaled_delta) / (1 - scaled_delta).
#'
#' `scaled_delta` is a real number constrained within the interval [-1, 1].
#' `scaled_delta` < 0 indicates dissortativity.
#' `scaled_delta` > 0 indicates assortativity.
#' `scaled_delta` = 0 indicates neutrality.
#'
#' `raw_delta` is a non-negative real number constrained within the interval [0, ∞).
#' `raw_delta` < 1 indicates dissortativity.
#' `raw_delta` > 1 indicates assortativity.
#' `raw_delta` = 1 indicates neutrality.
#'
#' @param scaled_delta A scaled numeric value to be reversed.
#' @return The original unscaled delta value.
#' @export
reverse_scale <- function(scaled_delta) {
  raw_delta <- (1 + scaled_delta) / (1 - scaled_delta)
  return(raw_delta)
}

# generate_sequences --------------------------------------------------------------------------

#' Generate DNA sequences based on given data, genome length, and mutation rate.
#'
#' This function generates DNA sequences based on the provided data, genome length, and mutation rate.
#' It iterates through each case in the data, generating a sequence by either randomly sampling nucleotides or mutating the sequence from a known source.
#'
#' @param data A data frame containing information about each case, including the source of infection (if known). Output of `simulate_groups()$data`.
#' @param genome_length An integer specifying the length of the DNA genome.
#' @param mutation_rate A numeric value representing the mutation rate per nucleotide per infection cycle.
#'
#' @return A character vector containing the generated DNA sequences.
#'
#' @export
#' @examples
#' data <- data.frame(
#'   id = c("case1", "case2", "case3"),
#'   source = c(NA, "case1", "case2")
#' )
#' generate_sequences(data, genome_length = 1000, mutation_rate = 0.001)
#'


generate_sequences <-
  function(data, genome_length, mutation_rate) {
    sequences <- matrix(nrow = nrow(data))
    no_source <- is.na(data$source)
    sequences[no_source] <- replicate(sum(no_source),
                                      paste(sample(
                                        c("A", "T", "C", "G"), genome_length, replace = TRUE
                                      ), collapse = ""))

    # Generate sequences for cases with a known source
    has_source <- !is.na(data$source)
    source_indices <- match(data$source, data$id)
    n_mut <-
      stats::rbinom(nrow(data), size = genome_length, prob = mutation_rate) #mutation_rate/genome_length?

    for (i in which(has_source)) {
      source_seq <- sequences[source_indices[i]]
      current_n_mut <- n_mut[i]

      if (current_n_mut > 0) {
        idx <-
          sample(1:genome_length, size = current_n_mut, replace = FALSE)
        mutated_seq <- strsplit(source_seq, "")[[1]]

        for (j in idx) {
          possible_nucleotides <-
            setdiff(c("A", "T", "G", "C"), mutated_seq[j])
          mutated_seq[j] <- sample(possible_nucleotides, size = 1)
        }
        sequences[i] <- paste(mutated_seq, collapse = "")
      } else {
        sequences[i] <- source_seq
      }
    }
    return(sequences)
  }



# simulate_groups_furrr -----------------------------------------------------------------------

#' simulate_groups_furrr
#'
#' Simulate multiple outbreaks in parallel. A furrr wrapper for `simulate_groups`.
#'
#' @param n_simulations The number of simulations to run.
#' @param n_cores The number of cores to use in parallel. Default is as.integer(future::availableCores() - 1).
#' @param duration An integer value representing the duration of the simulation.
#' @param n_groups An integer value representing the number of groups.
#' @param size A vector of integers representing the sizes of each group.
#' @param name A vector of characters representing the names of each group.
#' @param delta A numeric vector representing the within-group transmission factor (assortativity coefficient) for each group.
#' @param intro_n A vector of integers representing the number of introductions for each group.
#' @param r0 A numeric vector representing the basic reproductive number (R0) for each group.
#' @param generation_time The probability mass function (pmf) of the generation time.
#' @param incubation_period Optional. A vector of integers that will be sampled with replacement.
#' @param dt The time step of the simulation (default = 1 day).
#' @param stack Logical. If TRUE, returns a stacked dataframe by simulation ID else returns a list of `simulate_groups()` outputs. Default is TRUE.
#'
#' @return A list containing data frames with information about the simulated groups and the statistics of the simulation & matrices inputs (Mcol/M0).
#' @export
#'
#' @examples
#' \dontrun{
#' library(furrr)
#' simulate_groups_furrr(
#'   n_simulations = 20,
#'   n_cores = 5,
#'   duration = 100,
#'   n_groups = 3,
#'   size = c(100, 50, 200),
#'   name = c("A", "B", "C"),
#'   delta = c(1, 2, 5),
#'   intro_n = c(7, 5),
#'   r0 = c(2, 3, 4),
#'   generation_time = c(0.1, 0.3, 0.4, 0.2))
#' }
#'
#'


simulate_groups_furrr <- function(n_simulations,
                                  n_cores = as.integer(future::availableCores() - 1),
                                  duration,
                                  n_groups,
                                  size,
                                  name,
                                  delta,
                                  intro_n,
                                  r0,
                                  generation_time,
                                  incubation_period = NULL,
                                  dt = 1,
                                  stack = TRUE) {
  # Set up parallel processing using furrr
  future::plan("future::multisession", workers = n_cores)

  # Run the simulations in parallel
  results <- furrr::future_map(1:n_simulations, ~ {
    simulate_groups(
      duration = duration,
      n_groups = n_groups,
      size = size,
      name = name,
      delta = delta,
      intro_n = intro_n,
      r0 = r0,
      generation_time = generation_time,
      incubation_period = incubation_period,
      dt = dt
      )
  },
  .options = furrr::furrr_options(seed = TRUE),
  verbose = FALSE
  )


  if (isTRUE(stack)) {
    #stack simulations
    data <- purrr::map_dfr(results, ~ .x$data, .id = "simulation")
    stats <- purrr::map_dfr(results, ~ .x$stats, .id = "simulation")

    return(list(data = data, stats = stats))

  } else {
    return(results)
  }
}



